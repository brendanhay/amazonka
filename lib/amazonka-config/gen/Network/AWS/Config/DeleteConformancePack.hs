{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeleteConformancePack
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified conformance pack and all the AWS Config rules, remediation actions, and all evaluation results within that conformance pack.
--
--
-- AWS Config sets the conformance pack to @DELETE_IN_PROGRESS@ until the deletion is complete. You cannot update a conformance pack while it is in this state.
module Network.AWS.Config.DeleteConformancePack
  ( -- * Creating a Request
    deleteConformancePack,
    DeleteConformancePack,

    -- * Request Lenses
    dcpConformancePackName,

    -- * Destructuring the Response
    deleteConformancePackResponse,
    DeleteConformancePackResponse,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteConformancePack' smart constructor.
newtype DeleteConformancePack = DeleteConformancePack'
  { _dcpConformancePackName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteConformancePack' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpConformancePackName' - Name of the conformance pack you want to delete.
deleteConformancePack ::
  -- | 'dcpConformancePackName'
  Text ->
  DeleteConformancePack
deleteConformancePack pConformancePackName_ =
  DeleteConformancePack'
    { _dcpConformancePackName =
        pConformancePackName_
    }

-- | Name of the conformance pack you want to delete.
dcpConformancePackName :: Lens' DeleteConformancePack Text
dcpConformancePackName = lens _dcpConformancePackName (\s a -> s {_dcpConformancePackName = a})

instance AWSRequest DeleteConformancePack where
  type Rs DeleteConformancePack = DeleteConformancePackResponse
  request = postJSON config
  response = receiveNull DeleteConformancePackResponse'

instance Hashable DeleteConformancePack

instance NFData DeleteConformancePack

instance ToHeaders DeleteConformancePack where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StarlingDoveService.DeleteConformancePack" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteConformancePack where
  toJSON DeleteConformancePack' {..} =
    object
      ( catMaybes
          [Just ("ConformancePackName" .= _dcpConformancePackName)]
      )

instance ToPath DeleteConformancePack where
  toPath = const "/"

instance ToQuery DeleteConformancePack where
  toQuery = const mempty

-- | /See:/ 'deleteConformancePackResponse' smart constructor.
data DeleteConformancePackResponse = DeleteConformancePackResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteConformancePackResponse' with the minimum fields required to make a request.
deleteConformancePackResponse ::
  DeleteConformancePackResponse
deleteConformancePackResponse = DeleteConformancePackResponse'

instance NFData DeleteConformancePackResponse
