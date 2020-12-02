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
-- Module      : Network.AWS.EMR.PutBlockPublicAccessConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates an Amazon EMR block public access configuration for your AWS account in the current Region. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/configure-block-public-access.html Configure Block Public Access for Amazon EMR> in the /Amazon EMR Management Guide/ .
module Network.AWS.EMR.PutBlockPublicAccessConfiguration
  ( -- * Creating a Request
    putBlockPublicAccessConfiguration,
    PutBlockPublicAccessConfiguration,

    -- * Request Lenses
    pbpacBlockPublicAccessConfiguration,

    -- * Destructuring the Response
    putBlockPublicAccessConfigurationResponse,
    PutBlockPublicAccessConfigurationResponse,

    -- * Response Lenses
    pbpacrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putBlockPublicAccessConfiguration' smart constructor.
newtype PutBlockPublicAccessConfiguration = PutBlockPublicAccessConfiguration'
  { _pbpacBlockPublicAccessConfiguration ::
      BlockPublicAccessConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBlockPublicAccessConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbpacBlockPublicAccessConfiguration' - A configuration for Amazon EMR block public access. The configuration applies to all clusters created in your account for the current Region. The configuration specifies whether block public access is enabled. If block public access is enabled, security groups associated with the cluster cannot have rules that allow inbound traffic from 0.0.0.0/0 or ::/0 on a port, unless the port is specified as an exception using @PermittedPublicSecurityGroupRuleRanges@ in the @BlockPublicAccessConfiguration@ . By default, Port 22 (SSH) is an exception, and public access is allowed on this port. You can change this by updating @BlockPublicSecurityGroupRules@ to remove the exception.
putBlockPublicAccessConfiguration ::
  -- | 'pbpacBlockPublicAccessConfiguration'
  BlockPublicAccessConfiguration ->
  PutBlockPublicAccessConfiguration
putBlockPublicAccessConfiguration pBlockPublicAccessConfiguration_ =
  PutBlockPublicAccessConfiguration'
    { _pbpacBlockPublicAccessConfiguration =
        pBlockPublicAccessConfiguration_
    }

-- | A configuration for Amazon EMR block public access. The configuration applies to all clusters created in your account for the current Region. The configuration specifies whether block public access is enabled. If block public access is enabled, security groups associated with the cluster cannot have rules that allow inbound traffic from 0.0.0.0/0 or ::/0 on a port, unless the port is specified as an exception using @PermittedPublicSecurityGroupRuleRanges@ in the @BlockPublicAccessConfiguration@ . By default, Port 22 (SSH) is an exception, and public access is allowed on this port. You can change this by updating @BlockPublicSecurityGroupRules@ to remove the exception.
pbpacBlockPublicAccessConfiguration :: Lens' PutBlockPublicAccessConfiguration BlockPublicAccessConfiguration
pbpacBlockPublicAccessConfiguration = lens _pbpacBlockPublicAccessConfiguration (\s a -> s {_pbpacBlockPublicAccessConfiguration = a})

instance AWSRequest PutBlockPublicAccessConfiguration where
  type
    Rs PutBlockPublicAccessConfiguration =
      PutBlockPublicAccessConfigurationResponse
  request = postJSON emr
  response =
    receiveEmpty
      ( \s h x ->
          PutBlockPublicAccessConfigurationResponse' <$> (pure (fromEnum s))
      )

instance Hashable PutBlockPublicAccessConfiguration

instance NFData PutBlockPublicAccessConfiguration

instance ToHeaders PutBlockPublicAccessConfiguration where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "ElasticMapReduce.PutBlockPublicAccessConfiguration" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutBlockPublicAccessConfiguration where
  toJSON PutBlockPublicAccessConfiguration' {..} =
    object
      ( catMaybes
          [ Just
              ( "BlockPublicAccessConfiguration"
                  .= _pbpacBlockPublicAccessConfiguration
              )
          ]
      )

instance ToPath PutBlockPublicAccessConfiguration where
  toPath = const "/"

instance ToQuery PutBlockPublicAccessConfiguration where
  toQuery = const mempty

-- | /See:/ 'putBlockPublicAccessConfigurationResponse' smart constructor.
newtype PutBlockPublicAccessConfigurationResponse = PutBlockPublicAccessConfigurationResponse'
  { _pbpacrsResponseStatus ::
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

-- | Creates a value of 'PutBlockPublicAccessConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbpacrsResponseStatus' - -- | The response status code.
putBlockPublicAccessConfigurationResponse ::
  -- | 'pbpacrsResponseStatus'
  Int ->
  PutBlockPublicAccessConfigurationResponse
putBlockPublicAccessConfigurationResponse pResponseStatus_ =
  PutBlockPublicAccessConfigurationResponse'
    { _pbpacrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
pbpacrsResponseStatus :: Lens' PutBlockPublicAccessConfigurationResponse Int
pbpacrsResponseStatus = lens _pbpacrsResponseStatus (\s a -> s {_pbpacrsResponseStatus = a})

instance NFData PutBlockPublicAccessConfigurationResponse
