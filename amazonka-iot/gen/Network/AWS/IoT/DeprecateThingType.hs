{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.DeprecateThingType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deprecates a thing type. You can not associate new things with
-- deprecated thing type.
module Network.AWS.IoT.DeprecateThingType
  ( -- * Creating a Request
    DeprecateThingType (..),
    newDeprecateThingType,

    -- * Request Lenses
    deprecateThingType_undoDeprecate,
    deprecateThingType_thingTypeName,

    -- * Destructuring the Response
    DeprecateThingTypeResponse (..),
    newDeprecateThingTypeResponse,

    -- * Response Lenses
    deprecateThingTypeResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeprecateThingType operation.
--
-- /See:/ 'newDeprecateThingType' smart constructor.
data DeprecateThingType = DeprecateThingType'
  { -- | Whether to undeprecate a deprecated thing type. If __true__, the thing
    -- type will not be deprecated anymore and you can associate it with
    -- things.
    undoDeprecate :: Prelude.Maybe Prelude.Bool,
    -- | The name of the thing type to deprecate.
    thingTypeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeprecateThingType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'undoDeprecate', 'deprecateThingType_undoDeprecate' - Whether to undeprecate a deprecated thing type. If __true__, the thing
-- type will not be deprecated anymore and you can associate it with
-- things.
--
-- 'thingTypeName', 'deprecateThingType_thingTypeName' - The name of the thing type to deprecate.
newDeprecateThingType ::
  -- | 'thingTypeName'
  Prelude.Text ->
  DeprecateThingType
newDeprecateThingType pThingTypeName_ =
  DeprecateThingType'
    { undoDeprecate =
        Prelude.Nothing,
      thingTypeName = pThingTypeName_
    }

-- | Whether to undeprecate a deprecated thing type. If __true__, the thing
-- type will not be deprecated anymore and you can associate it with
-- things.
deprecateThingType_undoDeprecate :: Lens.Lens' DeprecateThingType (Prelude.Maybe Prelude.Bool)
deprecateThingType_undoDeprecate = Lens.lens (\DeprecateThingType' {undoDeprecate} -> undoDeprecate) (\s@DeprecateThingType' {} a -> s {undoDeprecate = a} :: DeprecateThingType)

-- | The name of the thing type to deprecate.
deprecateThingType_thingTypeName :: Lens.Lens' DeprecateThingType Prelude.Text
deprecateThingType_thingTypeName = Lens.lens (\DeprecateThingType' {thingTypeName} -> thingTypeName) (\s@DeprecateThingType' {} a -> s {thingTypeName = a} :: DeprecateThingType)

instance Prelude.AWSRequest DeprecateThingType where
  type
    Rs DeprecateThingType =
      DeprecateThingTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeprecateThingTypeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeprecateThingType

instance Prelude.NFData DeprecateThingType

instance Prelude.ToHeaders DeprecateThingType where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON DeprecateThingType where
  toJSON DeprecateThingType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("undoDeprecate" Prelude..=)
              Prelude.<$> undoDeprecate
          ]
      )

instance Prelude.ToPath DeprecateThingType where
  toPath DeprecateThingType' {..} =
    Prelude.mconcat
      [ "/thing-types/",
        Prelude.toBS thingTypeName,
        "/deprecate"
      ]

instance Prelude.ToQuery DeprecateThingType where
  toQuery = Prelude.const Prelude.mempty

-- | The output for the DeprecateThingType operation.
--
-- /See:/ 'newDeprecateThingTypeResponse' smart constructor.
data DeprecateThingTypeResponse = DeprecateThingTypeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeprecateThingTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deprecateThingTypeResponse_httpStatus' - The response's http status code.
newDeprecateThingTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeprecateThingTypeResponse
newDeprecateThingTypeResponse pHttpStatus_ =
  DeprecateThingTypeResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deprecateThingTypeResponse_httpStatus :: Lens.Lens' DeprecateThingTypeResponse Prelude.Int
deprecateThingTypeResponse_httpStatus = Lens.lens (\DeprecateThingTypeResponse' {httpStatus} -> httpStatus) (\s@DeprecateThingTypeResponse' {} a -> s {httpStatus = a} :: DeprecateThingTypeResponse)

instance Prelude.NFData DeprecateThingTypeResponse
