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
-- Module      : Network.AWS.SecretsManager.UntagResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes one or more tags from the specified secret.
--
--
-- This operation is idempotent. If a requested tag is not attached to the secret, no error is returned and the secret metadata is unchanged.
--
-- /Important:/ If you use tags as part of your security strategy, then removing a tag can change permissions. If successfully completing this operation would result in you losing your permissions for this secret, then the operation is blocked and returns an Access Denied error.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:UntagResource
--
--
--
-- __Related operations__
--
--     * To add one or more tags to the collection attached to a secret, use 'TagResource' .
--
--     * To view the list of tags attached to a secret, use 'DescribeSecret' .
--
--
--
module Network.AWS.SecretsManager.UntagResource
    (
    -- * Creating a Request
      untagResource
    , UntagResource
    -- * Request Lenses
    , urSecretId
    , urTagKeys

    -- * Destructuring the Response
    , untagResourceResponse
    , UntagResourceResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SecretsManager.Types
import Network.AWS.SecretsManager.Types.Product

-- | /See:/ 'untagResource' smart constructor.
data UntagResource = UntagResource'
  { _urSecretId :: !Text
  , _urTagKeys  :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UntagResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urSecretId' - The identifier for the secret that you want to remove tags from. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- * 'urTagKeys' - A list of tag key names to remove from the secret. You don't specify the value. Both the key and its associated value are removed. This parameter to the API requires a JSON text string argument. For information on how to format a JSON parameter for the various command line tool environments, see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ .
untagResource
    :: Text -- ^ 'urSecretId'
    -> UntagResource
untagResource pSecretId_ =
  UntagResource' {_urSecretId = pSecretId_, _urTagKeys = mempty}


-- | The identifier for the secret that you want to remove tags from. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
urSecretId :: Lens' UntagResource Text
urSecretId = lens _urSecretId (\ s a -> s{_urSecretId = a})

-- | A list of tag key names to remove from the secret. You don't specify the value. Both the key and its associated value are removed. This parameter to the API requires a JSON text string argument. For information on how to format a JSON parameter for the various command line tool environments, see <http://docs.aws.amazon.com/cli/latest/userguide/cli-using-param.html#cli-using-param-json Using JSON for Parameters> in the /AWS CLI User Guide/ .
urTagKeys :: Lens' UntagResource [Text]
urTagKeys = lens _urTagKeys (\ s a -> s{_urTagKeys = a}) . _Coerce

instance AWSRequest UntagResource where
        type Rs UntagResource = UntagResourceResponse
        request = postJSON secretsManager
        response = receiveNull UntagResourceResponse'

instance Hashable UntagResource where

instance NFData UntagResource where

instance ToHeaders UntagResource where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("secretsmanager.UntagResource" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UntagResource where
        toJSON UntagResource'{..}
          = object
              (catMaybes
                 [Just ("SecretId" .= _urSecretId),
                  Just ("TagKeys" .= _urTagKeys)])

instance ToPath UntagResource where
        toPath = const "/"

instance ToQuery UntagResource where
        toQuery = const mempty

-- | /See:/ 'untagResourceResponse' smart constructor.
data UntagResourceResponse =
  UntagResourceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UntagResourceResponse' with the minimum fields required to make a request.
--
untagResourceResponse
    :: UntagResourceResponse
untagResourceResponse = UntagResourceResponse'


instance NFData UntagResourceResponse where
