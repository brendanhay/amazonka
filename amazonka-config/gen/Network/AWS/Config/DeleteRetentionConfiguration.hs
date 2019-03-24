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
-- Module      : Network.AWS.Config.DeleteRetentionConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the retention configuration.
--
--
module Network.AWS.Config.DeleteRetentionConfiguration
    (
    -- * Creating a Request
      deleteRetentionConfiguration
    , DeleteRetentionConfiguration
    -- * Request Lenses
    , drcRetentionConfigurationName

    -- * Destructuring the Response
    , deleteRetentionConfigurationResponse
    , DeleteRetentionConfigurationResponse
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRetentionConfiguration' smart constructor.
newtype DeleteRetentionConfiguration = DeleteRetentionConfiguration'
  { _drcRetentionConfigurationName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRetentionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drcRetentionConfigurationName' - The name of the retention configuration to delete.
deleteRetentionConfiguration
    :: Text -- ^ 'drcRetentionConfigurationName'
    -> DeleteRetentionConfiguration
deleteRetentionConfiguration pRetentionConfigurationName_ =
  DeleteRetentionConfiguration'
    {_drcRetentionConfigurationName = pRetentionConfigurationName_}


-- | The name of the retention configuration to delete.
drcRetentionConfigurationName :: Lens' DeleteRetentionConfiguration Text
drcRetentionConfigurationName = lens _drcRetentionConfigurationName (\ s a -> s{_drcRetentionConfigurationName = a})

instance AWSRequest DeleteRetentionConfiguration
         where
        type Rs DeleteRetentionConfiguration =
             DeleteRetentionConfigurationResponse
        request = postJSON config
        response
          = receiveNull DeleteRetentionConfigurationResponse'

instance Hashable DeleteRetentionConfiguration where

instance NFData DeleteRetentionConfiguration where

instance ToHeaders DeleteRetentionConfiguration where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.DeleteRetentionConfiguration"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteRetentionConfiguration where
        toJSON DeleteRetentionConfiguration'{..}
          = object
              (catMaybes
                 [Just
                    ("RetentionConfigurationName" .=
                       _drcRetentionConfigurationName)])

instance ToPath DeleteRetentionConfiguration where
        toPath = const "/"

instance ToQuery DeleteRetentionConfiguration where
        toQuery = const mempty

-- | /See:/ 'deleteRetentionConfigurationResponse' smart constructor.
data DeleteRetentionConfigurationResponse =
  DeleteRetentionConfigurationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteRetentionConfigurationResponse' with the minimum fields required to make a request.
--
deleteRetentionConfigurationResponse
    :: DeleteRetentionConfigurationResponse
deleteRetentionConfigurationResponse = DeleteRetentionConfigurationResponse'


instance NFData DeleteRetentionConfigurationResponse
         where
