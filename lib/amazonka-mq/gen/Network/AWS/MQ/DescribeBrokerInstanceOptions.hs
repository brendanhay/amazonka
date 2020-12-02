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
-- Module      : Network.AWS.MQ.DescribeBrokerInstanceOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe available broker instance options.
module Network.AWS.MQ.DescribeBrokerInstanceOptions
  ( -- * Creating a Request
    describeBrokerInstanceOptions,
    DescribeBrokerInstanceOptions,

    -- * Request Lenses
    dbioNextToken,
    dbioEngineType,
    dbioMaxResults,
    dbioHostInstanceType,
    dbioStorageType,

    -- * Destructuring the Response
    describeBrokerInstanceOptionsResponse,
    DescribeBrokerInstanceOptionsResponse,

    -- * Response Lenses
    dbiorsNextToken,
    dbiorsBrokerInstanceOptions,
    dbiorsMaxResults,
    dbiorsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MQ.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeBrokerInstanceOptions' smart constructor.
data DescribeBrokerInstanceOptions = DescribeBrokerInstanceOptions'
  { _dbioNextToken ::
      !(Maybe Text),
    _dbioEngineType ::
      !(Maybe Text),
    _dbioMaxResults :: !(Maybe Nat),
    _dbioHostInstanceType ::
      !(Maybe Text),
    _dbioStorageType ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeBrokerInstanceOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbioNextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- * 'dbioEngineType' - Filter response by engine type.
--
-- * 'dbioMaxResults' - The maximum number of instance options that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
--
-- * 'dbioHostInstanceType' - Filter response by host instance type.
--
-- * 'dbioStorageType' - Filter response by storage type.
describeBrokerInstanceOptions ::
  DescribeBrokerInstanceOptions
describeBrokerInstanceOptions =
  DescribeBrokerInstanceOptions'
    { _dbioNextToken = Nothing,
      _dbioEngineType = Nothing,
      _dbioMaxResults = Nothing,
      _dbioHostInstanceType = Nothing,
      _dbioStorageType = Nothing
    }

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
dbioNextToken :: Lens' DescribeBrokerInstanceOptions (Maybe Text)
dbioNextToken = lens _dbioNextToken (\s a -> s {_dbioNextToken = a})

-- | Filter response by engine type.
dbioEngineType :: Lens' DescribeBrokerInstanceOptions (Maybe Text)
dbioEngineType = lens _dbioEngineType (\s a -> s {_dbioEngineType = a})

-- | The maximum number of instance options that Amazon MQ can return per page (20 by default). This value must be an integer from 5 to 100.
dbioMaxResults :: Lens' DescribeBrokerInstanceOptions (Maybe Natural)
dbioMaxResults = lens _dbioMaxResults (\s a -> s {_dbioMaxResults = a}) . mapping _Nat

-- | Filter response by host instance type.
dbioHostInstanceType :: Lens' DescribeBrokerInstanceOptions (Maybe Text)
dbioHostInstanceType = lens _dbioHostInstanceType (\s a -> s {_dbioHostInstanceType = a})

-- | Filter response by storage type.
dbioStorageType :: Lens' DescribeBrokerInstanceOptions (Maybe Text)
dbioStorageType = lens _dbioStorageType (\s a -> s {_dbioStorageType = a})

instance AWSRequest DescribeBrokerInstanceOptions where
  type
    Rs DescribeBrokerInstanceOptions =
      DescribeBrokerInstanceOptionsResponse
  request = get mq
  response =
    receiveJSON
      ( \s h x ->
          DescribeBrokerInstanceOptionsResponse'
            <$> (x .?> "nextToken")
            <*> (x .?> "brokerInstanceOptions" .!@ mempty)
            <*> (x .?> "maxResults")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeBrokerInstanceOptions

instance NFData DescribeBrokerInstanceOptions

instance ToHeaders DescribeBrokerInstanceOptions where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToPath DescribeBrokerInstanceOptions where
  toPath = const "/v1/broker-instance-options"

instance ToQuery DescribeBrokerInstanceOptions where
  toQuery DescribeBrokerInstanceOptions' {..} =
    mconcat
      [ "nextToken" =: _dbioNextToken,
        "engineType" =: _dbioEngineType,
        "maxResults" =: _dbioMaxResults,
        "hostInstanceType" =: _dbioHostInstanceType,
        "storageType" =: _dbioStorageType
      ]

-- | /See:/ 'describeBrokerInstanceOptionsResponse' smart constructor.
data DescribeBrokerInstanceOptionsResponse = DescribeBrokerInstanceOptionsResponse'
  { _dbiorsNextToken ::
      !(Maybe Text),
    _dbiorsBrokerInstanceOptions ::
      !( Maybe
           [BrokerInstanceOption]
       ),
    _dbiorsMaxResults ::
      !(Maybe Nat),
    _dbiorsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeBrokerInstanceOptionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbiorsNextToken' - The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
--
-- * 'dbiorsBrokerInstanceOptions' - List of available broker instance options.
--
-- * 'dbiorsMaxResults' - Required. The maximum number of instance options that can be returned per page (20 by default). This value must be an integer from 5 to 100.
--
-- * 'dbiorsResponseStatus' - -- | The response status code.
describeBrokerInstanceOptionsResponse ::
  -- | 'dbiorsResponseStatus'
  Int ->
  DescribeBrokerInstanceOptionsResponse
describeBrokerInstanceOptionsResponse pResponseStatus_ =
  DescribeBrokerInstanceOptionsResponse'
    { _dbiorsNextToken =
        Nothing,
      _dbiorsBrokerInstanceOptions = Nothing,
      _dbiorsMaxResults = Nothing,
      _dbiorsResponseStatus = pResponseStatus_
    }

-- | The token that specifies the next page of results Amazon MQ should return. To request the first page, leave nextToken empty.
dbiorsNextToken :: Lens' DescribeBrokerInstanceOptionsResponse (Maybe Text)
dbiorsNextToken = lens _dbiorsNextToken (\s a -> s {_dbiorsNextToken = a})

-- | List of available broker instance options.
dbiorsBrokerInstanceOptions :: Lens' DescribeBrokerInstanceOptionsResponse [BrokerInstanceOption]
dbiorsBrokerInstanceOptions = lens _dbiorsBrokerInstanceOptions (\s a -> s {_dbiorsBrokerInstanceOptions = a}) . _Default . _Coerce

-- | Required. The maximum number of instance options that can be returned per page (20 by default). This value must be an integer from 5 to 100.
dbiorsMaxResults :: Lens' DescribeBrokerInstanceOptionsResponse (Maybe Natural)
dbiorsMaxResults = lens _dbiorsMaxResults (\s a -> s {_dbiorsMaxResults = a}) . mapping _Nat

-- | -- | The response status code.
dbiorsResponseStatus :: Lens' DescribeBrokerInstanceOptionsResponse Int
dbiorsResponseStatus = lens _dbiorsResponseStatus (\s a -> s {_dbiorsResponseStatus = a})

instance NFData DescribeBrokerInstanceOptionsResponse
