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
-- Module      : Network.AWS.MQ.DescribeBrokerEngineTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe available engine types and versions.
module Network.AWS.MQ.DescribeBrokerEngineTypes
  ( -- * Creating a Request
    describeBrokerEngineTypes,
    DescribeBrokerEngineTypes,

    -- * Request Lenses
    dbetNextToken,
    dbetEngineType,
    dbetMaxResults,

    -- * Destructuring the Response
    describeBrokerEngineTypesResponse,
    DescribeBrokerEngineTypesResponse,

    -- * Response Lenses
    dbetrsBrokerEngineTypes,
    dbetrsNextToken,
    dbetrsMaxResults,
    dbetrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MQ.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeBrokerEngineTypes' smart constructor.
data DescribeBrokerEngineTypes = DescribeBrokerEngineTypes'
  { _dbetNextToken ::
      !(Maybe Text),
    _dbetEngineType :: !(Maybe Text),
    _dbetMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeBrokerEngineTypes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbetNextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- * 'dbetEngineType' - Filter response by engine type.
--
-- * 'dbetMaxResults' - The maximum number of engine types that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
describeBrokerEngineTypes ::
  DescribeBrokerEngineTypes
describeBrokerEngineTypes =
  DescribeBrokerEngineTypes'
    { _dbetNextToken = Nothing,
      _dbetEngineType = Nothing,
      _dbetMaxResults = Nothing
    }

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
dbetNextToken :: Lens' DescribeBrokerEngineTypes (Maybe Text)
dbetNextToken = lens _dbetNextToken (\s a -> s {_dbetNextToken = a})

-- | Filter response by engine type.
dbetEngineType :: Lens' DescribeBrokerEngineTypes (Maybe Text)
dbetEngineType = lens _dbetEngineType (\s a -> s {_dbetEngineType = a})

-- | The maximum number of engine types that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
dbetMaxResults :: Lens' DescribeBrokerEngineTypes (Maybe Natural)
dbetMaxResults = lens _dbetMaxResults (\s a -> s {_dbetMaxResults = a}) . mapping _Nat

instance AWSRequest DescribeBrokerEngineTypes where
  type
    Rs DescribeBrokerEngineTypes =
      DescribeBrokerEngineTypesResponse
  request = get mq
  response =
    receiveJSON
      ( \s h x ->
          DescribeBrokerEngineTypesResponse'
            <$> (x .?> "brokerEngineTypes" .!@ mempty)
            <*> (x .?> "nextToken")
            <*> (x .?> "maxResults")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeBrokerEngineTypes

instance NFData DescribeBrokerEngineTypes

instance ToHeaders DescribeBrokerEngineTypes where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DescribeBrokerEngineTypes where
  toPath = const "/v1/broker-engine-types"

instance ToQuery DescribeBrokerEngineTypes where
  toQuery DescribeBrokerEngineTypes' {..} =
    mconcat
      [ "nextToken" =: _dbetNextToken,
        "engineType" =: _dbetEngineType,
        "maxResults" =: _dbetMaxResults
      ]

-- | /See:/ 'describeBrokerEngineTypesResponse' smart constructor.
data DescribeBrokerEngineTypesResponse = DescribeBrokerEngineTypesResponse'
  { _dbetrsBrokerEngineTypes ::
      !( Maybe
           [BrokerEngineType]
       ),
    _dbetrsNextToken ::
      !(Maybe Text),
    _dbetrsMaxResults ::
      !(Maybe Nat),
    _dbetrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeBrokerEngineTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbetrsBrokerEngineTypes' - List of available engine types and versions.
--
-- * 'dbetrsNextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- * 'dbetrsMaxResults' - Required. The maximum number of engine types that can be returned per page (20 by default). This value must be an integer from 5 to 100.
--
-- * 'dbetrsResponseStatus' - -- | The response status code.
describeBrokerEngineTypesResponse ::
  -- | 'dbetrsResponseStatus'
  Int ->
  DescribeBrokerEngineTypesResponse
describeBrokerEngineTypesResponse pResponseStatus_ =
  DescribeBrokerEngineTypesResponse'
    { _dbetrsBrokerEngineTypes =
        Nothing,
      _dbetrsNextToken = Nothing,
      _dbetrsMaxResults = Nothing,
      _dbetrsResponseStatus = pResponseStatus_
    }

-- | List of available engine types and versions.
dbetrsBrokerEngineTypes :: Lens' DescribeBrokerEngineTypesResponse [BrokerEngineType]
dbetrsBrokerEngineTypes = lens _dbetrsBrokerEngineTypes (\s a -> s {_dbetrsBrokerEngineTypes = a}) . _Default . _Coerce

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
dbetrsNextToken :: Lens' DescribeBrokerEngineTypesResponse (Maybe Text)
dbetrsNextToken = lens _dbetrsNextToken (\s a -> s {_dbetrsNextToken = a})

-- | Required. The maximum number of engine types that can be returned per page (20 by default). This value must be an integer from 5 to 100.
dbetrsMaxResults :: Lens' DescribeBrokerEngineTypesResponse (Maybe Natural)
dbetrsMaxResults = lens _dbetrsMaxResults (\s a -> s {_dbetrsMaxResults = a}) . mapping _Nat

-- | -- | The response status code.
dbetrsResponseStatus :: Lens' DescribeBrokerEngineTypesResponse Int
dbetrsResponseStatus = lens _dbetrsResponseStatus (\s a -> s {_dbetrsResponseStatus = a})

instance NFData DescribeBrokerEngineTypesResponse
