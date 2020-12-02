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
-- Module      : Network.AWS.EMR.DeleteSecurityConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a security configuration.
--
--
module Network.AWS.EMR.DeleteSecurityConfiguration
    (
    -- * Creating a Request
      deleteSecurityConfiguration
    , DeleteSecurityConfiguration
    -- * Request Lenses
    , dscName

    -- * Destructuring the Response
    , deleteSecurityConfigurationResponse
    , DeleteSecurityConfigurationResponse
    -- * Response Lenses
    , dscrsResponseStatus
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSecurityConfiguration' smart constructor.
newtype DeleteSecurityConfiguration = DeleteSecurityConfiguration'
  { _dscName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSecurityConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscName' - The name of the security configuration.
deleteSecurityConfiguration
    :: Text -- ^ 'dscName'
    -> DeleteSecurityConfiguration
deleteSecurityConfiguration pName_ =
  DeleteSecurityConfiguration' {_dscName = pName_}


-- | The name of the security configuration.
dscName :: Lens' DeleteSecurityConfiguration Text
dscName = lens _dscName (\ s a -> s{_dscName = a})

instance AWSRequest DeleteSecurityConfiguration where
        type Rs DeleteSecurityConfiguration =
             DeleteSecurityConfigurationResponse
        request = postJSON emr
        response
          = receiveEmpty
              (\ s h x ->
                 DeleteSecurityConfigurationResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DeleteSecurityConfiguration where

instance NFData DeleteSecurityConfiguration where

instance ToHeaders DeleteSecurityConfiguration where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.DeleteSecurityConfiguration" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteSecurityConfiguration where
        toJSON DeleteSecurityConfiguration'{..}
          = object (catMaybes [Just ("Name" .= _dscName)])

instance ToPath DeleteSecurityConfiguration where
        toPath = const "/"

instance ToQuery DeleteSecurityConfiguration where
        toQuery = const mempty

-- | /See:/ 'deleteSecurityConfigurationResponse' smart constructor.
newtype DeleteSecurityConfigurationResponse = DeleteSecurityConfigurationResponse'
  { _dscrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSecurityConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dscrsResponseStatus' - -- | The response status code.
deleteSecurityConfigurationResponse
    :: Int -- ^ 'dscrsResponseStatus'
    -> DeleteSecurityConfigurationResponse
deleteSecurityConfigurationResponse pResponseStatus_ =
  DeleteSecurityConfigurationResponse' {_dscrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dscrsResponseStatus :: Lens' DeleteSecurityConfigurationResponse Int
dscrsResponseStatus = lens _dscrsResponseStatus (\ s a -> s{_dscrsResponseStatus = a})

instance NFData DeleteSecurityConfigurationResponse
         where
