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
-- Module      : Amazonka.RobOMaker.CreateFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a fleet, a logical group of robots running the same robot
-- application.
module Amazonka.RobOMaker.CreateFleet
  ( -- * Creating a Request
    CreateFleet (..),
    newCreateFleet,

    -- * Request Lenses
    createFleet_tags,
    createFleet_name,

    -- * Destructuring the Response
    CreateFleetResponse (..),
    newCreateFleetResponse,

    -- * Response Lenses
    createFleetResponse_arn,
    createFleetResponse_createdAt,
    createFleetResponse_name,
    createFleetResponse_tags,
    createFleetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newCreateFleet' smart constructor.
data CreateFleet = CreateFleet'
  { -- | A map that contains tag keys and tag values that are attached to the
    -- fleet.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the fleet.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createFleet_tags' - A map that contains tag keys and tag values that are attached to the
-- fleet.
--
-- 'name', 'createFleet_name' - The name of the fleet.
newCreateFleet ::
  -- | 'name'
  Prelude.Text ->
  CreateFleet
newCreateFleet pName_ =
  CreateFleet' {tags = Prelude.Nothing, name = pName_}

-- | A map that contains tag keys and tag values that are attached to the
-- fleet.
createFleet_tags :: Lens.Lens' CreateFleet (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createFleet_tags = Lens.lens (\CreateFleet' {tags} -> tags) (\s@CreateFleet' {} a -> s {tags = a} :: CreateFleet) Prelude.. Lens.mapping Lens.coerced

-- | The name of the fleet.
createFleet_name :: Lens.Lens' CreateFleet Prelude.Text
createFleet_name = Lens.lens (\CreateFleet' {name} -> name) (\s@CreateFleet' {} a -> s {name = a} :: CreateFleet)

instance Core.AWSRequest CreateFleet where
  type AWSResponse CreateFleet = CreateFleetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFleetResponse'
            Prelude.<$> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "createdAt")
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFleet where
  hashWithSalt _salt CreateFleet' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateFleet where
  rnf CreateFleet' {..} =
    Prelude.rnf tags `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders CreateFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateFleet where
  toJSON CreateFleet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateFleet where
  toPath = Prelude.const "/createFleet"

instance Core.ToQuery CreateFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFleetResponse' smart constructor.
data CreateFleetResponse = CreateFleetResponse'
  { -- | The Amazon Resource Name (ARN) of the fleet.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the fleet was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The name of the fleet.
    name :: Prelude.Maybe Prelude.Text,
    -- | The list of all tags added to the fleet.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createFleetResponse_arn' - The Amazon Resource Name (ARN) of the fleet.
--
-- 'createdAt', 'createFleetResponse_createdAt' - The time, in milliseconds since the epoch, when the fleet was created.
--
-- 'name', 'createFleetResponse_name' - The name of the fleet.
--
-- 'tags', 'createFleetResponse_tags' - The list of all tags added to the fleet.
--
-- 'httpStatus', 'createFleetResponse_httpStatus' - The response's http status code.
newCreateFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFleetResponse
newCreateFleetResponse pHttpStatus_ =
  CreateFleetResponse'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the fleet.
createFleetResponse_arn :: Lens.Lens' CreateFleetResponse (Prelude.Maybe Prelude.Text)
createFleetResponse_arn = Lens.lens (\CreateFleetResponse' {arn} -> arn) (\s@CreateFleetResponse' {} a -> s {arn = a} :: CreateFleetResponse)

-- | The time, in milliseconds since the epoch, when the fleet was created.
createFleetResponse_createdAt :: Lens.Lens' CreateFleetResponse (Prelude.Maybe Prelude.UTCTime)
createFleetResponse_createdAt = Lens.lens (\CreateFleetResponse' {createdAt} -> createdAt) (\s@CreateFleetResponse' {} a -> s {createdAt = a} :: CreateFleetResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the fleet.
createFleetResponse_name :: Lens.Lens' CreateFleetResponse (Prelude.Maybe Prelude.Text)
createFleetResponse_name = Lens.lens (\CreateFleetResponse' {name} -> name) (\s@CreateFleetResponse' {} a -> s {name = a} :: CreateFleetResponse)

-- | The list of all tags added to the fleet.
createFleetResponse_tags :: Lens.Lens' CreateFleetResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createFleetResponse_tags = Lens.lens (\CreateFleetResponse' {tags} -> tags) (\s@CreateFleetResponse' {} a -> s {tags = a} :: CreateFleetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createFleetResponse_httpStatus :: Lens.Lens' CreateFleetResponse Prelude.Int
createFleetResponse_httpStatus = Lens.lens (\CreateFleetResponse' {httpStatus} -> httpStatus) (\s@CreateFleetResponse' {} a -> s {httpStatus = a} :: CreateFleetResponse)

instance Prelude.NFData CreateFleetResponse where
  rnf CreateFleetResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
