{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Outposts.CreateOutpost
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Outpost.
--
-- You can specify @AvailabilityZone@ or @AvailabilityZoneId@.
module Amazonka.Outposts.CreateOutpost
  ( -- * Creating a Request
    CreateOutpost (..),
    newCreateOutpost,

    -- * Request Lenses
    createOutpost_availabilityZoneId,
    createOutpost_availabilityZone,
    createOutpost_description,
    createOutpost_tags,
    createOutpost_name,
    createOutpost_siteId,

    -- * Destructuring the Response
    CreateOutpostResponse (..),
    newCreateOutpostResponse,

    -- * Response Lenses
    createOutpostResponse_outpost,
    createOutpostResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Outposts.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateOutpost' smart constructor.
data CreateOutpost = CreateOutpost'
  { availabilityZoneId :: Prelude.Maybe Prelude.Text,
    availabilityZone :: Prelude.Maybe Prelude.Text,
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags to apply to the Outpost.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    name :: Prelude.Text,
    siteId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOutpost' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZoneId', 'createOutpost_availabilityZoneId' - Undocumented member.
--
-- 'availabilityZone', 'createOutpost_availabilityZone' - Undocumented member.
--
-- 'description', 'createOutpost_description' - Undocumented member.
--
-- 'tags', 'createOutpost_tags' - The tags to apply to the Outpost.
--
-- 'name', 'createOutpost_name' - Undocumented member.
--
-- 'siteId', 'createOutpost_siteId' - Undocumented member.
newCreateOutpost ::
  -- | 'name'
  Prelude.Text ->
  -- | 'siteId'
  Prelude.Text ->
  CreateOutpost
newCreateOutpost pName_ pSiteId_ =
  CreateOutpost'
    { availabilityZoneId =
        Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      siteId = pSiteId_
    }

-- | Undocumented member.
createOutpost_availabilityZoneId :: Lens.Lens' CreateOutpost (Prelude.Maybe Prelude.Text)
createOutpost_availabilityZoneId = Lens.lens (\CreateOutpost' {availabilityZoneId} -> availabilityZoneId) (\s@CreateOutpost' {} a -> s {availabilityZoneId = a} :: CreateOutpost)

-- | Undocumented member.
createOutpost_availabilityZone :: Lens.Lens' CreateOutpost (Prelude.Maybe Prelude.Text)
createOutpost_availabilityZone = Lens.lens (\CreateOutpost' {availabilityZone} -> availabilityZone) (\s@CreateOutpost' {} a -> s {availabilityZone = a} :: CreateOutpost)

-- | Undocumented member.
createOutpost_description :: Lens.Lens' CreateOutpost (Prelude.Maybe Prelude.Text)
createOutpost_description = Lens.lens (\CreateOutpost' {description} -> description) (\s@CreateOutpost' {} a -> s {description = a} :: CreateOutpost)

-- | The tags to apply to the Outpost.
createOutpost_tags :: Lens.Lens' CreateOutpost (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createOutpost_tags = Lens.lens (\CreateOutpost' {tags} -> tags) (\s@CreateOutpost' {} a -> s {tags = a} :: CreateOutpost) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createOutpost_name :: Lens.Lens' CreateOutpost Prelude.Text
createOutpost_name = Lens.lens (\CreateOutpost' {name} -> name) (\s@CreateOutpost' {} a -> s {name = a} :: CreateOutpost)

-- | Undocumented member.
createOutpost_siteId :: Lens.Lens' CreateOutpost Prelude.Text
createOutpost_siteId = Lens.lens (\CreateOutpost' {siteId} -> siteId) (\s@CreateOutpost' {} a -> s {siteId = a} :: CreateOutpost)

instance Core.AWSRequest CreateOutpost where
  type
    AWSResponse CreateOutpost =
      CreateOutpostResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateOutpostResponse'
            Prelude.<$> (x Core..?> "Outpost")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateOutpost where
  hashWithSalt salt' CreateOutpost' {..} =
    salt' `Prelude.hashWithSalt` siteId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` availabilityZoneId

instance Prelude.NFData CreateOutpost where
  rnf CreateOutpost' {..} =
    Prelude.rnf availabilityZoneId
      `Prelude.seq` Prelude.rnf siteId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf availabilityZone

instance Core.ToHeaders CreateOutpost where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateOutpost where
  toJSON CreateOutpost' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AvailabilityZoneId" Core..=)
              Prelude.<$> availabilityZoneId,
            ("AvailabilityZone" Core..=)
              Prelude.<$> availabilityZone,
            ("Description" Core..=) Prelude.<$> description,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("SiteId" Core..= siteId)
          ]
      )

instance Core.ToPath CreateOutpost where
  toPath = Prelude.const "/outposts"

instance Core.ToQuery CreateOutpost where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateOutpostResponse' smart constructor.
data CreateOutpostResponse = CreateOutpostResponse'
  { outpost :: Prelude.Maybe Outpost,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateOutpostResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outpost', 'createOutpostResponse_outpost' - Undocumented member.
--
-- 'httpStatus', 'createOutpostResponse_httpStatus' - The response's http status code.
newCreateOutpostResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateOutpostResponse
newCreateOutpostResponse pHttpStatus_ =
  CreateOutpostResponse'
    { outpost = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createOutpostResponse_outpost :: Lens.Lens' CreateOutpostResponse (Prelude.Maybe Outpost)
createOutpostResponse_outpost = Lens.lens (\CreateOutpostResponse' {outpost} -> outpost) (\s@CreateOutpostResponse' {} a -> s {outpost = a} :: CreateOutpostResponse)

-- | The response's http status code.
createOutpostResponse_httpStatus :: Lens.Lens' CreateOutpostResponse Prelude.Int
createOutpostResponse_httpStatus = Lens.lens (\CreateOutpostResponse' {httpStatus} -> httpStatus) (\s@CreateOutpostResponse' {} a -> s {httpStatus = a} :: CreateOutpostResponse)

instance Prelude.NFData CreateOutpostResponse where
  rnf CreateOutpostResponse' {..} =
    Prelude.rnf outpost
      `Prelude.seq` Prelude.rnf httpStatus
