{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.RemoveLayerVersionPermission
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a statement from the permissions policy for a version of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> . For more information, see 'AddLayerVersionPermission' .
--
--
module Network.AWS.Lambda.RemoveLayerVersionPermission
    (
    -- * Creating a Request
      removeLayerVersionPermission
    , RemoveLayerVersionPermission
    -- * Request Lenses
    , rlvpRevisionId
    , rlvpLayerName
    , rlvpVersionNumber
    , rlvpStatementId

    -- * Destructuring the Response
    , removeLayerVersionPermissionResponse
    , RemoveLayerVersionPermissionResponse
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeLayerVersionPermission' smart constructor.
data RemoveLayerVersionPermission = RemoveLayerVersionPermission'
  { _rlvpRevisionId    :: !(Maybe Text)
  , _rlvpLayerName     :: !Text
  , _rlvpVersionNumber :: !Integer
  , _rlvpStatementId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveLayerVersionPermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rlvpRevisionId' - Only update the policy if the revision ID matches the ID specified. Use this option to avoid modifying a policy that has changed since you last read it.
--
-- * 'rlvpLayerName' - The name or Amazon Resource Name (ARN) of the layer.
--
-- * 'rlvpVersionNumber' - The version number.
--
-- * 'rlvpStatementId' - The identifier that was specified when the statement was added.
removeLayerVersionPermission
    :: Text -- ^ 'rlvpLayerName'
    -> Integer -- ^ 'rlvpVersionNumber'
    -> Text -- ^ 'rlvpStatementId'
    -> RemoveLayerVersionPermission
removeLayerVersionPermission pLayerName_ pVersionNumber_ pStatementId_ =
  RemoveLayerVersionPermission'
    { _rlvpRevisionId = Nothing
    , _rlvpLayerName = pLayerName_
    , _rlvpVersionNumber = pVersionNumber_
    , _rlvpStatementId = pStatementId_
    }


-- | Only update the policy if the revision ID matches the ID specified. Use this option to avoid modifying a policy that has changed since you last read it.
rlvpRevisionId :: Lens' RemoveLayerVersionPermission (Maybe Text)
rlvpRevisionId = lens _rlvpRevisionId (\ s a -> s{_rlvpRevisionId = a})

-- | The name or Amazon Resource Name (ARN) of the layer.
rlvpLayerName :: Lens' RemoveLayerVersionPermission Text
rlvpLayerName = lens _rlvpLayerName (\ s a -> s{_rlvpLayerName = a})

-- | The version number.
rlvpVersionNumber :: Lens' RemoveLayerVersionPermission Integer
rlvpVersionNumber = lens _rlvpVersionNumber (\ s a -> s{_rlvpVersionNumber = a})

-- | The identifier that was specified when the statement was added.
rlvpStatementId :: Lens' RemoveLayerVersionPermission Text
rlvpStatementId = lens _rlvpStatementId (\ s a -> s{_rlvpStatementId = a})

instance AWSRequest RemoveLayerVersionPermission
         where
        type Rs RemoveLayerVersionPermission =
             RemoveLayerVersionPermissionResponse
        request = delete lambda
        response
          = receiveNull RemoveLayerVersionPermissionResponse'

instance Hashable RemoveLayerVersionPermission where

instance NFData RemoveLayerVersionPermission where

instance ToHeaders RemoveLayerVersionPermission where
        toHeaders = const mempty

instance ToPath RemoveLayerVersionPermission where
        toPath RemoveLayerVersionPermission'{..}
          = mconcat
              ["/2018-10-31/layers/", toBS _rlvpLayerName,
               "/versions/", toBS _rlvpVersionNumber, "/policy/",
               toBS _rlvpStatementId]

instance ToQuery RemoveLayerVersionPermission where
        toQuery RemoveLayerVersionPermission'{..}
          = mconcat ["RevisionId" =: _rlvpRevisionId]

-- | /See:/ 'removeLayerVersionPermissionResponse' smart constructor.
data RemoveLayerVersionPermissionResponse =
  RemoveLayerVersionPermissionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveLayerVersionPermissionResponse' with the minimum fields required to make a request.
--
removeLayerVersionPermissionResponse
    :: RemoveLayerVersionPermissionResponse
removeLayerVersionPermissionResponse = RemoveLayerVersionPermissionResponse'


instance NFData RemoveLayerVersionPermissionResponse
         where
