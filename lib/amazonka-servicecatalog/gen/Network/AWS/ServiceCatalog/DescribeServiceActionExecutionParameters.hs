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
-- Module      : Network.AWS.ServiceCatalog.DescribeServiceActionExecutionParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Finds the default parameters for a specific self-service action on a specific provisioned product and returns a map of the results to the user.
module Network.AWS.ServiceCatalog.DescribeServiceActionExecutionParameters
  ( -- * Creating a Request
    describeServiceActionExecutionParameters,
    DescribeServiceActionExecutionParameters,

    -- * Request Lenses
    dsaepAcceptLanguage,
    dsaepProvisionedProductId,
    dsaepServiceActionId,

    -- * Destructuring the Response
    describeServiceActionExecutionParametersResponse,
    DescribeServiceActionExecutionParametersResponse,

    -- * Response Lenses
    dsaeprsServiceActionParameters,
    dsaeprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.ServiceCatalog.Types

-- | /See:/ 'describeServiceActionExecutionParameters' smart constructor.
data DescribeServiceActionExecutionParameters = DescribeServiceActionExecutionParameters'
  { _dsaepAcceptLanguage ::
      !( Maybe
           Text
       ),
    _dsaepProvisionedProductId ::
      !Text,
    _dsaepServiceActionId ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeServiceActionExecutionParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsaepAcceptLanguage' - The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
--
-- * 'dsaepProvisionedProductId' - The identifier of the provisioned product.
--
-- * 'dsaepServiceActionId' - The self-service action identifier.
describeServiceActionExecutionParameters ::
  -- | 'dsaepProvisionedProductId'
  Text ->
  -- | 'dsaepServiceActionId'
  Text ->
  DescribeServiceActionExecutionParameters
describeServiceActionExecutionParameters
  pProvisionedProductId_
  pServiceActionId_ =
    DescribeServiceActionExecutionParameters'
      { _dsaepAcceptLanguage =
          Nothing,
        _dsaepProvisionedProductId = pProvisionedProductId_,
        _dsaepServiceActionId = pServiceActionId_
      }

-- | The language code.     * @en@ - English (default)     * @jp@ - Japanese     * @zh@ - Chinese
dsaepAcceptLanguage :: Lens' DescribeServiceActionExecutionParameters (Maybe Text)
dsaepAcceptLanguage = lens _dsaepAcceptLanguage (\s a -> s {_dsaepAcceptLanguage = a})

-- | The identifier of the provisioned product.
dsaepProvisionedProductId :: Lens' DescribeServiceActionExecutionParameters Text
dsaepProvisionedProductId = lens _dsaepProvisionedProductId (\s a -> s {_dsaepProvisionedProductId = a})

-- | The self-service action identifier.
dsaepServiceActionId :: Lens' DescribeServiceActionExecutionParameters Text
dsaepServiceActionId = lens _dsaepServiceActionId (\s a -> s {_dsaepServiceActionId = a})

instance AWSRequest DescribeServiceActionExecutionParameters where
  type
    Rs DescribeServiceActionExecutionParameters =
      DescribeServiceActionExecutionParametersResponse
  request = postJSON serviceCatalog
  response =
    receiveJSON
      ( \s h x ->
          DescribeServiceActionExecutionParametersResponse'
            <$> (x .?> "ServiceActionParameters" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeServiceActionExecutionParameters

instance NFData DescribeServiceActionExecutionParameters

instance ToHeaders DescribeServiceActionExecutionParameters where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWS242ServiceCatalogService.DescribeServiceActionExecutionParameters" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeServiceActionExecutionParameters where
  toJSON DescribeServiceActionExecutionParameters' {..} =
    object
      ( catMaybes
          [ ("AcceptLanguage" .=) <$> _dsaepAcceptLanguage,
            Just ("ProvisionedProductId" .= _dsaepProvisionedProductId),
            Just ("ServiceActionId" .= _dsaepServiceActionId)
          ]
      )

instance ToPath DescribeServiceActionExecutionParameters where
  toPath = const "/"

instance ToQuery DescribeServiceActionExecutionParameters where
  toQuery = const mempty

-- | /See:/ 'describeServiceActionExecutionParametersResponse' smart constructor.
data DescribeServiceActionExecutionParametersResponse = DescribeServiceActionExecutionParametersResponse'
  { _dsaeprsServiceActionParameters ::
      !( Maybe
           [ExecutionParameter]
       ),
    _dsaeprsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeServiceActionExecutionParametersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsaeprsServiceActionParameters' - The parameters of the self-service action.
--
-- * 'dsaeprsResponseStatus' - -- | The response status code.
describeServiceActionExecutionParametersResponse ::
  -- | 'dsaeprsResponseStatus'
  Int ->
  DescribeServiceActionExecutionParametersResponse
describeServiceActionExecutionParametersResponse pResponseStatus_ =
  DescribeServiceActionExecutionParametersResponse'
    { _dsaeprsServiceActionParameters =
        Nothing,
      _dsaeprsResponseStatus = pResponseStatus_
    }

-- | The parameters of the self-service action.
dsaeprsServiceActionParameters :: Lens' DescribeServiceActionExecutionParametersResponse [ExecutionParameter]
dsaeprsServiceActionParameters = lens _dsaeprsServiceActionParameters (\s a -> s {_dsaeprsServiceActionParameters = a}) . _Default . _Coerce

-- | -- | The response status code.
dsaeprsResponseStatus :: Lens' DescribeServiceActionExecutionParametersResponse Int
dsaeprsResponseStatus = lens _dsaeprsResponseStatus (\s a -> s {_dsaeprsResponseStatus = a})

instance NFData DescribeServiceActionExecutionParametersResponse
