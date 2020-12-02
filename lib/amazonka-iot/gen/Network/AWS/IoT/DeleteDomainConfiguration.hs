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
-- Module      : Network.AWS.IoT.DeleteDomainConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified domain configuration.
module Network.AWS.IoT.DeleteDomainConfiguration
  ( -- * Creating a Request
    deleteDomainConfiguration,
    DeleteDomainConfiguration,

    -- * Request Lenses
    dDomainConfigurationName,

    -- * Destructuring the Response
    deleteDomainConfigurationResponse,
    DeleteDomainConfigurationResponse,

    -- * Response Lenses
    ddcdrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteDomainConfiguration' smart constructor.
newtype DeleteDomainConfiguration = DeleteDomainConfiguration'
  { _dDomainConfigurationName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDomainConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dDomainConfigurationName' - The name of the domain configuration to be deleted.
deleteDomainConfiguration ::
  -- | 'dDomainConfigurationName'
  Text ->
  DeleteDomainConfiguration
deleteDomainConfiguration pDomainConfigurationName_ =
  DeleteDomainConfiguration'
    { _dDomainConfigurationName =
        pDomainConfigurationName_
    }

-- | The name of the domain configuration to be deleted.
dDomainConfigurationName :: Lens' DeleteDomainConfiguration Text
dDomainConfigurationName = lens _dDomainConfigurationName (\s a -> s {_dDomainConfigurationName = a})

instance AWSRequest DeleteDomainConfiguration where
  type
    Rs DeleteDomainConfiguration =
      DeleteDomainConfigurationResponse
  request = delete ioT
  response =
    receiveEmpty
      ( \s h x ->
          DeleteDomainConfigurationResponse' <$> (pure (fromEnum s))
      )

instance Hashable DeleteDomainConfiguration

instance NFData DeleteDomainConfiguration

instance ToHeaders DeleteDomainConfiguration where
  toHeaders = const mempty

instance ToPath DeleteDomainConfiguration where
  toPath DeleteDomainConfiguration' {..} =
    mconcat
      ["/domainConfigurations/", toBS _dDomainConfigurationName]

instance ToQuery DeleteDomainConfiguration where
  toQuery = const mempty

-- | /See:/ 'deleteDomainConfigurationResponse' smart constructor.
newtype DeleteDomainConfigurationResponse = DeleteDomainConfigurationResponse'
  { _ddcdrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteDomainConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddcdrsResponseStatus' - -- | The response status code.
deleteDomainConfigurationResponse ::
  -- | 'ddcdrsResponseStatus'
  Int ->
  DeleteDomainConfigurationResponse
deleteDomainConfigurationResponse pResponseStatus_ =
  DeleteDomainConfigurationResponse'
    { _ddcdrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
ddcdrsResponseStatus :: Lens' DeleteDomainConfigurationResponse Int
ddcdrsResponseStatus = lens _ddcdrsResponseStatus (\s a -> s {_ddcdrsResponseStatus = a})

instance NFData DeleteDomainConfigurationResponse
