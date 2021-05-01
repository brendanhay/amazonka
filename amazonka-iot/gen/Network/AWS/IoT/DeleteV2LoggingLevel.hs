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
-- Module      : Network.AWS.IoT.DeleteV2LoggingLevel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a logging level.
module Network.AWS.IoT.DeleteV2LoggingLevel
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteV2LoggingLevel' smart constructor.
data DeleteV2LoggingLevel = DeleteV2LoggingLevel'
  { -- | The type of resource for which you are configuring logging. Must be
    -- @THING_Group@.
    targetType :: LogTargetType,
    -- | The name of the resource for which you are configuring logging.
    targetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteV2LoggingLevel where
  type
    Rs DeleteV2LoggingLevel =
      DeleteV2LoggingLevelResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteV2LoggingLevelResponse'

instance Prelude.Hashable DeleteV2LoggingLevel

instance Prelude.NFData DeleteV2LoggingLevel

instance Prelude.ToHeaders DeleteV2LoggingLevel where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteV2LoggingLevel where
  toPath = Prelude.const "/v2LoggingLevel"

instance Prelude.ToQuery DeleteV2LoggingLevel where
  toQuery DeleteV2LoggingLevel' {..} =
    Prelude.mconcat
      [ "targetType" Prelude.=: targetType,
        "targetName" Prelude.=: targetName
      ]

-- | /See:/ 'newDeleteV2LoggingLevelResponse' smart constructor.
data DeleteV2LoggingLevelResponse = DeleteV2LoggingLevelResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteV2LoggingLevelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteV2LoggingLevelResponse ::
  DeleteV2LoggingLevelResponse
newDeleteV2LoggingLevelResponse =
  DeleteV2LoggingLevelResponse'

instance Prelude.NFData DeleteV2LoggingLevelResponse
