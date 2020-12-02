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
-- Module      : Network.AWS.Config.DeleteRemediationConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the remediation configuration.
module Network.AWS.Config.DeleteRemediationConfiguration
  ( -- * Creating a Request
    deleteRemediationConfiguration,
    DeleteRemediationConfiguration,

    -- * Request Lenses
    delResourceType,
    delConfigRuleName,

    -- * Destructuring the Response
    deleteRemediationConfigurationResponse,
    DeleteRemediationConfigurationResponse,

    -- * Response Lenses
    drcrsResponseStatus,
  )
where

import Network.AWS.Config.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRemediationConfiguration' smart constructor.
data DeleteRemediationConfiguration = DeleteRemediationConfiguration'
  { _delResourceType ::
      !(Maybe Text),
    _delConfigRuleName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteRemediationConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delResourceType' - The type of a resource.
--
-- * 'delConfigRuleName' - The name of the AWS Config rule for which you want to delete remediation configuration.
deleteRemediationConfiguration ::
  -- | 'delConfigRuleName'
  Text ->
  DeleteRemediationConfiguration
deleteRemediationConfiguration pConfigRuleName_ =
  DeleteRemediationConfiguration'
    { _delResourceType = Nothing,
      _delConfigRuleName = pConfigRuleName_
    }

-- | The type of a resource.
delResourceType :: Lens' DeleteRemediationConfiguration (Maybe Text)
delResourceType = lens _delResourceType (\s a -> s {_delResourceType = a})

-- | The name of the AWS Config rule for which you want to delete remediation configuration.
delConfigRuleName :: Lens' DeleteRemediationConfiguration Text
delConfigRuleName = lens _delConfigRuleName (\s a -> s {_delConfigRuleName = a})

instance AWSRequest DeleteRemediationConfiguration where
  type
    Rs DeleteRemediationConfiguration =
      DeleteRemediationConfigurationResponse
  request = postJSON config
  response =
    receiveEmpty
      ( \s h x ->
          DeleteRemediationConfigurationResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteRemediationConfiguration

instance NFData DeleteRemediationConfiguration

instance ToHeaders DeleteRemediationConfiguration where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "StarlingDoveService.DeleteRemediationConfiguration" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteRemediationConfiguration where
  toJSON DeleteRemediationConfiguration' {..} =
    object
      ( catMaybes
          [ ("ResourceType" .=) <$> _delResourceType,
            Just ("ConfigRuleName" .= _delConfigRuleName)
          ]
      )

instance ToPath DeleteRemediationConfiguration where
  toPath = const "/"

instance ToQuery DeleteRemediationConfiguration where
  toQuery = const mempty

-- | /See:/ 'deleteRemediationConfigurationResponse' smart constructor.
newtype DeleteRemediationConfigurationResponse = DeleteRemediationConfigurationResponse'
  { _drcrsResponseStatus ::
      Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DeleteRemediationConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drcrsResponseStatus' - -- | The response status code.
deleteRemediationConfigurationResponse ::
  -- | 'drcrsResponseStatus'
  Int ->
  DeleteRemediationConfigurationResponse
deleteRemediationConfigurationResponse pResponseStatus_ =
  DeleteRemediationConfigurationResponse'
    { _drcrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
drcrsResponseStatus :: Lens' DeleteRemediationConfigurationResponse Int
drcrsResponseStatus = lens _drcrsResponseStatus (\s a -> s {_drcrsResponseStatus = a})

instance NFData DeleteRemediationConfigurationResponse
