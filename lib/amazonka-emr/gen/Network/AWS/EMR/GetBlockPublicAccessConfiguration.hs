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
-- Module      : Network.AWS.EMR.GetBlockPublicAccessConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the Amazon EMR block public access configuration for your AWS account in the current Region. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/configure-block-public-access.html Configure Block Public Access for Amazon EMR> in the /Amazon EMR Management Guide/ .
module Network.AWS.EMR.GetBlockPublicAccessConfiguration
  ( -- * Creating a Request
    getBlockPublicAccessConfiguration,
    GetBlockPublicAccessConfiguration,

    -- * Destructuring the Response
    getBlockPublicAccessConfigurationResponse,
    GetBlockPublicAccessConfigurationResponse,

    -- * Response Lenses
    gbpacrsResponseStatus,
    gbpacrsBlockPublicAccessConfiguration,
    gbpacrsBlockPublicAccessConfigurationMetadata,
  )
where

import Network.AWS.EMR.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getBlockPublicAccessConfiguration' smart constructor.
data GetBlockPublicAccessConfiguration = GetBlockPublicAccessConfiguration'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBlockPublicAccessConfiguration' with the minimum fields required to make a request.
getBlockPublicAccessConfiguration ::
  GetBlockPublicAccessConfiguration
getBlockPublicAccessConfiguration =
  GetBlockPublicAccessConfiguration'

instance AWSRequest GetBlockPublicAccessConfiguration where
  type
    Rs GetBlockPublicAccessConfiguration =
      GetBlockPublicAccessConfigurationResponse
  request = postJSON emr
  response =
    receiveJSON
      ( \s h x ->
          GetBlockPublicAccessConfigurationResponse'
            <$> (pure (fromEnum s))
            <*> (x .:> "BlockPublicAccessConfiguration")
            <*> (x .:> "BlockPublicAccessConfigurationMetadata")
      )

instance Hashable GetBlockPublicAccessConfiguration

instance NFData GetBlockPublicAccessConfiguration

instance ToHeaders GetBlockPublicAccessConfiguration where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "ElasticMapReduce.GetBlockPublicAccessConfiguration" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetBlockPublicAccessConfiguration where
  toJSON = const (Object mempty)

instance ToPath GetBlockPublicAccessConfiguration where
  toPath = const "/"

instance ToQuery GetBlockPublicAccessConfiguration where
  toQuery = const mempty

-- | /See:/ 'getBlockPublicAccessConfigurationResponse' smart constructor.
data GetBlockPublicAccessConfigurationResponse = GetBlockPublicAccessConfigurationResponse'
  { _gbpacrsResponseStatus ::
      !Int,
    _gbpacrsBlockPublicAccessConfiguration ::
      !BlockPublicAccessConfiguration,
    _gbpacrsBlockPublicAccessConfigurationMetadata ::
      !BlockPublicAccessConfigurationMetadata
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'GetBlockPublicAccessConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbpacrsResponseStatus' - -- | The response status code.
--
-- * 'gbpacrsBlockPublicAccessConfiguration' - A configuration for Amazon EMR block public access. The configuration applies to all clusters created in your account for the current Region. The configuration specifies whether block public access is enabled. If block public access is enabled, security groups associated with the cluster cannot have rules that allow inbound traffic from 0.0.0.0/0 or ::/0 on a port, unless the port is specified as an exception using @PermittedPublicSecurityGroupRuleRanges@ in the @BlockPublicAccessConfiguration@ . By default, Port 22 (SSH) is an exception, and public access is allowed on this port. You can change this by updating the block public access configuration to remove the exception.
--
-- * 'gbpacrsBlockPublicAccessConfigurationMetadata' - Properties that describe the AWS principal that created the @BlockPublicAccessConfiguration@ using the @PutBlockPublicAccessConfiguration@ action as well as the date and time that the configuration was created. Each time a configuration for block public access is updated, Amazon EMR updates this metadata.
getBlockPublicAccessConfigurationResponse ::
  -- | 'gbpacrsResponseStatus'
  Int ->
  -- | 'gbpacrsBlockPublicAccessConfiguration'
  BlockPublicAccessConfiguration ->
  -- | 'gbpacrsBlockPublicAccessConfigurationMetadata'
  BlockPublicAccessConfigurationMetadata ->
  GetBlockPublicAccessConfigurationResponse
getBlockPublicAccessConfigurationResponse
  pResponseStatus_
  pBlockPublicAccessConfiguration_
  pBlockPublicAccessConfigurationMetadata_ =
    GetBlockPublicAccessConfigurationResponse'
      { _gbpacrsResponseStatus =
          pResponseStatus_,
        _gbpacrsBlockPublicAccessConfiguration =
          pBlockPublicAccessConfiguration_,
        _gbpacrsBlockPublicAccessConfigurationMetadata =
          pBlockPublicAccessConfigurationMetadata_
      }

-- | -- | The response status code.
gbpacrsResponseStatus :: Lens' GetBlockPublicAccessConfigurationResponse Int
gbpacrsResponseStatus = lens _gbpacrsResponseStatus (\s a -> s {_gbpacrsResponseStatus = a})

-- | A configuration for Amazon EMR block public access. The configuration applies to all clusters created in your account for the current Region. The configuration specifies whether block public access is enabled. If block public access is enabled, security groups associated with the cluster cannot have rules that allow inbound traffic from 0.0.0.0/0 or ::/0 on a port, unless the port is specified as an exception using @PermittedPublicSecurityGroupRuleRanges@ in the @BlockPublicAccessConfiguration@ . By default, Port 22 (SSH) is an exception, and public access is allowed on this port. You can change this by updating the block public access configuration to remove the exception.
gbpacrsBlockPublicAccessConfiguration :: Lens' GetBlockPublicAccessConfigurationResponse BlockPublicAccessConfiguration
gbpacrsBlockPublicAccessConfiguration = lens _gbpacrsBlockPublicAccessConfiguration (\s a -> s {_gbpacrsBlockPublicAccessConfiguration = a})

-- | Properties that describe the AWS principal that created the @BlockPublicAccessConfiguration@ using the @PutBlockPublicAccessConfiguration@ action as well as the date and time that the configuration was created. Each time a configuration for block public access is updated, Amazon EMR updates this metadata.
gbpacrsBlockPublicAccessConfigurationMetadata :: Lens' GetBlockPublicAccessConfigurationResponse BlockPublicAccessConfigurationMetadata
gbpacrsBlockPublicAccessConfigurationMetadata = lens _gbpacrsBlockPublicAccessConfigurationMetadata (\s a -> s {_gbpacrsBlockPublicAccessConfigurationMetadata = a})

instance NFData GetBlockPublicAccessConfigurationResponse
