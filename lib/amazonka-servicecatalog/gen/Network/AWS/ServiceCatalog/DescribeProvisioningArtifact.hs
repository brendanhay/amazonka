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
-- Module      : Network.AWS.ServiceCatalog.DescribeProvisioningArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified provisioning artifact (also known as a version) for the specified product.
module Network.AWS.ServiceCatalog.DescribeProvisioningArtifact
  ( -- * Creating a Request
    describeProvisioningArtifact,
    DescribeProvisioningArtifact,

    -- * Request Lenses
    dpaProductName,
    dpaProvisioningArtifactId,
    dpaVerbose,
    dpaProvisioningArtifactName,
    dpaAcceptLanguage,
    dpaProductId,

    -- * Destructuring the Response
    describeProvisioningArtifactResponse,
    DescribeProvisioningArtifactResponse,

    -- * Response Lenses
    dpaprsStatus,
    dpaprsInfo,
    dpaprsProvisioningArtifactDetail,
    dpaprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'describeProvisioningArtifact' smart constructor.
data DescribeProvisioningArtifact = DescribeProvisioningArtifact'
  { _dpaProductName ::
      !(Maybe Text),
    _dpaProvisioningArtifactId ::
      !(Maybe Text),
    _dpaVerbose :: !(Maybe Bool),
    _dpaProvisioningArtifactName ::
      !(Maybe Text),
    _dpaAcceptLanguage ::
      !(Maybe Text),
    _dpaProductId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeProvisioningArtifact' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpaProductName' - The product name.
--
-- * 'dpaProvisioningArtifactId' - The identifier of the provisioning artifact.
--
-- * 'dpaVerbose' - Indicates whether a verbose level of detail is enabled.
--
-- * 'dpaProvisioningArtifactName' - The provisioning artifact name.
--
-- * 'dpaAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'dpaProductId' - The product identifier.
describeProvisioningArtifact ::
  DescribeProvisioningArtifact
describeProvisioningArtifact =
  DescribeProvisioningArtifact'
    { _dpaProductName = Nothing,
      _dpaProvisioningArtifactId = Nothing,
      _dpaVerbose = Nothing,
      _dpaProvisioningArtifactName = Nothing,
      _dpaAcceptLanguage = Nothing,
      _dpaProductId = Nothing
    }

-- | The product name.
dpaProductName :: Lens' DescribeProvisioningArtifact (Maybe Text)
dpaProductName = lens _dpaProductName (\s a -> s {_dpaProductName = a})

-- | The identifier of the provisioning artifact.
dpaProvisioningArtifactId :: Lens' DescribeProvisioningArtifact (Maybe Text)
dpaProvisioningArtifactId = lens _dpaProvisioningArtifactId (\s a -> s {_dpaProvisioningArtifactId = a})

-- | Indicates whether a verbose level of detail is enabled.
dpaVerbose :: Lens' DescribeProvisioningArtifact (Maybe Bool)
dpaVerbose = lens _dpaVerbose (\s a -> s {_dpaVerbose = a})

-- | The provisioning artifact name.
dpaProvisioningArtifactName :: Lens' DescribeProvisioningArtifact (Maybe Text)
dpaProvisioningArtifactName = lens _dpaProvisioningArtifactName (\s a -> s {_dpaProvisioningArtifactName = a})

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
dpaAcceptLanguage :: Lens' DescribeProvisioningArtifact (Maybe Text)
dpaAcceptLanguage = lens _dpaAcceptLanguage (\s a -> s {_dpaAcceptLanguage = a})

-- | The product identifier.
dpaProductId :: Lens' DescribeProvisioningArtifact (Maybe Text)
dpaProductId = lens _dpaProductId (\s a -> s {_dpaProductId = a})

instance AWSRequest DescribeProvisioningArtifact where
  type
    Rs DescribeProvisioningArtifact =
      DescribeProvisioningArtifactResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          DescribeProvisioningArtifactResponse'
            <$> (x .?> "Status")
            <*> (x .?> "Info" .!@ mempty)
            <*> (x .?> "ProvisioningArtifactDetail")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeProvisioningArtifact

instance NFData DescribeProvisioningArtifact

instance ToHeaders DescribeProvisioningArtifact where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWS242ServiceCatalogService.DescribeProvisioningArtifact" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeProvisioningArtifact where
  toJSON DescribeProvisioningArtifact' {..} =
    object
      ( catMaybes
          [ ("ProductName" .=) <$> _dpaProductName,
            ("ProvisioningArtifactId" .=) <$> _dpaProvisioningArtifactId,
            ("Verbose" .=) <$> _dpaVerbose,
            ("ProvisioningArtifactName" .=) <$> _dpaProvisioningArtifactName,
            ("AcceptLanguage" .=) <$> _dpaAcceptLanguage,
            ("ProductId" .=) <$> _dpaProductId
          ]
      )

instance ToPath DescribeProvisioningArtifact where
  toPath = const "/"

instance ToQuery DescribeProvisioningArtifact where
  toQuery = const mempty

-- | /See:/ 'describeProvisioningArtifactResponse' smart constructor.
data DescribeProvisioningArtifactResponse = DescribeProvisioningArtifactResponse'
  { _dpaprsStatus ::
      !( Maybe
           RequestStatus
       ),
    _dpaprsInfo ::
      !( Maybe
           ( Map
               Text
               (Text)
           )
       ),
    _dpaprsProvisioningArtifactDetail ::
      !( Maybe
           ProvisioningArtifactDetail
       ),
    _dpaprsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeProvisioningArtifactResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpaprsStatus' - The status of the current request.
--
-- * 'dpaprsInfo' - The URL of the CloudFormation template in Amazon S3.
--
-- * 'dpaprsProvisioningArtifactDetail' - Information about the provisioning artifact.
--
-- * 'dpaprsResponseStatus' - -- | The response status code.
describeProvisioningArtifactResponse ::
  -- | 'dpaprsResponseStatus'
  Int ->
  DescribeProvisioningArtifactResponse
describeProvisioningArtifactResponse pResponseStatus_ =
  DescribeProvisioningArtifactResponse'
    { _dpaprsStatus = Nothing,
      _dpaprsInfo = Nothing,
      _dpaprsProvisioningArtifactDetail = Nothing,
      _dpaprsResponseStatus = pResponseStatus_
    }

-- | The status of the current request.
dpaprsStatus :: Lens' DescribeProvisioningArtifactResponse (Maybe RequestStatus)
dpaprsStatus = lens _dpaprsStatus (\s a -> s {_dpaprsStatus = a})

-- | The URL of the CloudFormation template in Amazon S3.
dpaprsInfo :: Lens' DescribeProvisioningArtifactResponse (HashMap Text (Text))
dpaprsInfo = lens _dpaprsInfo (\s a -> s {_dpaprsInfo = a}) . _Default . _Map

-- | Information about the provisioning artifact.
dpaprsProvisioningArtifactDetail :: Lens' DescribeProvisioningArtifactResponse (Maybe ProvisioningArtifactDetail)
dpaprsProvisioningArtifactDetail = lens _dpaprsProvisioningArtifactDetail (\s a -> s {_dpaprsProvisioningArtifactDetail = a})

-- | -- | The response status code.
dpaprsResponseStatus :: Lens' DescribeProvisioningArtifactResponse Int
dpaprsResponseStatus = lens _dpaprsResponseStatus (\s a -> s {_dpaprsResponseStatus = a})

instance NFData DescribeProvisioningArtifactResponse
