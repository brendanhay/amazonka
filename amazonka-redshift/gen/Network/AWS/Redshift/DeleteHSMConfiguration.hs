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
-- Module      : Network.AWS.Redshift.DeleteHSMConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Amazon Redshift HSM configuration.
--
--
module Network.AWS.Redshift.DeleteHSMConfiguration
    (
    -- * Creating a Request
      deleteHSMConfiguration
    , DeleteHSMConfiguration
    -- * Request Lenses
    , dhcHSMConfigurationIdentifier

    -- * Destructuring the Response
    , deleteHSMConfigurationResponse
    , DeleteHSMConfigurationResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Redshift.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'deleteHSMConfiguration' smart constructor.
newtype DeleteHSMConfiguration = DeleteHSMConfiguration'
  { _dhcHSMConfigurationIdentifier :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteHSMConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhcHSMConfigurationIdentifier' - The identifier of the Amazon Redshift HSM configuration to be deleted.
deleteHSMConfiguration
    :: Text -- ^ 'dhcHSMConfigurationIdentifier'
    -> DeleteHSMConfiguration
deleteHSMConfiguration pHSMConfigurationIdentifier_ =
  DeleteHSMConfiguration'
    {_dhcHSMConfigurationIdentifier = pHSMConfigurationIdentifier_}


-- | The identifier of the Amazon Redshift HSM configuration to be deleted.
dhcHSMConfigurationIdentifier :: Lens' DeleteHSMConfiguration Text
dhcHSMConfigurationIdentifier = lens _dhcHSMConfigurationIdentifier (\ s a -> s{_dhcHSMConfigurationIdentifier = a})

instance AWSRequest DeleteHSMConfiguration where
        type Rs DeleteHSMConfiguration =
             DeleteHSMConfigurationResponse
        request = postQuery redshift
        response
          = receiveNull DeleteHSMConfigurationResponse'

instance Hashable DeleteHSMConfiguration where

instance NFData DeleteHSMConfiguration where

instance ToHeaders DeleteHSMConfiguration where
        toHeaders = const mempty

instance ToPath DeleteHSMConfiguration where
        toPath = const "/"

instance ToQuery DeleteHSMConfiguration where
        toQuery DeleteHSMConfiguration'{..}
          = mconcat
              ["Action" =:
                 ("DeleteHsmConfiguration" :: ByteString),
               "Version" =: ("2012-12-01" :: ByteString),
               "HsmConfigurationIdentifier" =:
                 _dhcHSMConfigurationIdentifier]

-- | /See:/ 'deleteHSMConfigurationResponse' smart constructor.
data DeleteHSMConfigurationResponse =
  DeleteHSMConfigurationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteHSMConfigurationResponse' with the minimum fields required to make a request.
--
deleteHSMConfigurationResponse
    :: DeleteHSMConfigurationResponse
deleteHSMConfigurationResponse = DeleteHSMConfigurationResponse'


instance NFData DeleteHSMConfigurationResponse where
