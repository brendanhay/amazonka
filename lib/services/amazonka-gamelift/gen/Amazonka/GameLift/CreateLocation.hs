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
-- Module      : Amazonka.GameLift.CreateLocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom location for use in an Anywhere fleet.
module Amazonka.GameLift.CreateLocation
  ( -- * Creating a Request
    CreateLocation (..),
    newCreateLocation,

    -- * Request Lenses
    createLocation_tags,
    createLocation_locationName,

    -- * Destructuring the Response
    CreateLocationResponse (..),
    newCreateLocationResponse,

    -- * Response Lenses
    createLocationResponse_location,
    createLocationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLocation' smart constructor.
data CreateLocation = CreateLocation'
  { -- | A list of labels to assign to the new matchmaking configuration
    -- resource. Tags are developer-defined key-value pairs. Tagging Amazon Web
    -- Services resources are useful for resource management, access management
    -- and cost allocation. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
    -- in the /Amazon Web Services General Rareference/.
    tags :: Prelude.Maybe [Tag],
    -- | A descriptive name for the custom location.
    locationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createLocation_tags' - A list of labels to assign to the new matchmaking configuration
-- resource. Tags are developer-defined key-value pairs. Tagging Amazon Web
-- Services resources are useful for resource management, access management
-- and cost allocation. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in the /Amazon Web Services General Rareference/.
--
-- 'locationName', 'createLocation_locationName' - A descriptive name for the custom location.
newCreateLocation ::
  -- | 'locationName'
  Prelude.Text ->
  CreateLocation
newCreateLocation pLocationName_ =
  CreateLocation'
    { tags = Prelude.Nothing,
      locationName = pLocationName_
    }

-- | A list of labels to assign to the new matchmaking configuration
-- resource. Tags are developer-defined key-value pairs. Tagging Amazon Web
-- Services resources are useful for resource management, access management
-- and cost allocation. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>
-- in the /Amazon Web Services General Rareference/.
createLocation_tags :: Lens.Lens' CreateLocation (Prelude.Maybe [Tag])
createLocation_tags = Lens.lens (\CreateLocation' {tags} -> tags) (\s@CreateLocation' {} a -> s {tags = a} :: CreateLocation) Prelude.. Lens.mapping Lens.coerced

-- | A descriptive name for the custom location.
createLocation_locationName :: Lens.Lens' CreateLocation Prelude.Text
createLocation_locationName = Lens.lens (\CreateLocation' {locationName} -> locationName) (\s@CreateLocation' {} a -> s {locationName = a} :: CreateLocation)

instance Core.AWSRequest CreateLocation where
  type
    AWSResponse CreateLocation =
      CreateLocationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLocationResponse'
            Prelude.<$> (x Data..?> "Location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLocation where
  hashWithSalt _salt CreateLocation' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` locationName

instance Prelude.NFData CreateLocation where
  rnf CreateLocation' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf locationName

instance Data.ToHeaders CreateLocation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("GameLift.CreateLocation" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLocation where
  toJSON CreateLocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("LocationName" Data..= locationName)
          ]
      )

instance Data.ToPath CreateLocation where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLocation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLocationResponse' smart constructor.
data CreateLocationResponse = CreateLocationResponse'
  { -- | The details of the custom location you created.
    location :: Prelude.Maybe LocationModel,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'location', 'createLocationResponse_location' - The details of the custom location you created.
--
-- 'httpStatus', 'createLocationResponse_httpStatus' - The response's http status code.
newCreateLocationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLocationResponse
newCreateLocationResponse pHttpStatus_ =
  CreateLocationResponse'
    { location = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the custom location you created.
createLocationResponse_location :: Lens.Lens' CreateLocationResponse (Prelude.Maybe LocationModel)
createLocationResponse_location = Lens.lens (\CreateLocationResponse' {location} -> location) (\s@CreateLocationResponse' {} a -> s {location = a} :: CreateLocationResponse)

-- | The response's http status code.
createLocationResponse_httpStatus :: Lens.Lens' CreateLocationResponse Prelude.Int
createLocationResponse_httpStatus = Lens.lens (\CreateLocationResponse' {httpStatus} -> httpStatus) (\s@CreateLocationResponse' {} a -> s {httpStatus = a} :: CreateLocationResponse)

instance Prelude.NFData CreateLocationResponse where
  rnf CreateLocationResponse' {..} =
    Prelude.rnf location
      `Prelude.seq` Prelude.rnf httpStatus
