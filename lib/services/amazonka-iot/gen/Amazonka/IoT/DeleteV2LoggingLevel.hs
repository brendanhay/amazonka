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
-- Module      : Amazonka.IoT.DeleteV2LoggingLevel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a logging level.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteV2LoggingLevel>
-- action.
module Amazonka.IoT.DeleteV2LoggingLevel
  ( -- * Creating a Request
    DeleteV2LoggingLevel (..),
    newDeleteV2LoggingLevel,

    -- * Request Lenses
    deleteV2LoggingLevel_targetType,
    deleteV2LoggingLevel_targetName,

    -- * Destructuring the Response
    DeleteV2LoggingLevelResponse (..),
    newDeleteV2LoggingLevelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteV2LoggingLevel' smart constructor.
data DeleteV2LoggingLevel = DeleteV2LoggingLevel'
  { -- | The type of resource for which you are configuring logging. Must be
    -- @THING_Group@.
    targetType :: LogTargetType,
    -- | The name of the resource for which you are configuring logging.
    targetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteV2LoggingLevel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetType', 'deleteV2LoggingLevel_targetType' - The type of resource for which you are configuring logging. Must be
-- @THING_Group@.
--
-- 'targetName', 'deleteV2LoggingLevel_targetName' - The name of the resource for which you are configuring logging.
newDeleteV2LoggingLevel ::
  -- | 'targetType'
  LogTargetType ->
  -- | 'targetName'
  Prelude.Text ->
  DeleteV2LoggingLevel
newDeleteV2LoggingLevel pTargetType_ pTargetName_ =
  DeleteV2LoggingLevel'
    { targetType = pTargetType_,
      targetName = pTargetName_
    }

-- | The type of resource for which you are configuring logging. Must be
-- @THING_Group@.
deleteV2LoggingLevel_targetType :: Lens.Lens' DeleteV2LoggingLevel LogTargetType
deleteV2LoggingLevel_targetType = Lens.lens (\DeleteV2LoggingLevel' {targetType} -> targetType) (\s@DeleteV2LoggingLevel' {} a -> s {targetType = a} :: DeleteV2LoggingLevel)

-- | The name of the resource for which you are configuring logging.
deleteV2LoggingLevel_targetName :: Lens.Lens' DeleteV2LoggingLevel Prelude.Text
deleteV2LoggingLevel_targetName = Lens.lens (\DeleteV2LoggingLevel' {targetName} -> targetName) (\s@DeleteV2LoggingLevel' {} a -> s {targetName = a} :: DeleteV2LoggingLevel)

instance Core.AWSRequest DeleteV2LoggingLevel where
  type
    AWSResponse DeleteV2LoggingLevel =
      DeleteV2LoggingLevelResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteV2LoggingLevelResponse'

instance Prelude.Hashable DeleteV2LoggingLevel where
  hashWithSalt _salt DeleteV2LoggingLevel' {..} =
    _salt
      `Prelude.hashWithSalt` targetType
      `Prelude.hashWithSalt` targetName

instance Prelude.NFData DeleteV2LoggingLevel where
  rnf DeleteV2LoggingLevel' {..} =
    Prelude.rnf targetType
      `Prelude.seq` Prelude.rnf targetName

instance Data.ToHeaders DeleteV2LoggingLevel where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteV2LoggingLevel where
  toPath = Prelude.const "/v2LoggingLevel"

instance Data.ToQuery DeleteV2LoggingLevel where
  toQuery DeleteV2LoggingLevel' {..} =
    Prelude.mconcat
      [ "targetType" Data.=: targetType,
        "targetName" Data.=: targetName
      ]

-- | /See:/ 'newDeleteV2LoggingLevelResponse' smart constructor.
data DeleteV2LoggingLevelResponse = DeleteV2LoggingLevelResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteV2LoggingLevelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteV2LoggingLevelResponse ::
  DeleteV2LoggingLevelResponse
newDeleteV2LoggingLevelResponse =
  DeleteV2LoggingLevelResponse'

instance Prelude.NFData DeleteV2LoggingLevelResponse where
  rnf _ = ()
