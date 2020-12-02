{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ElasticsearchAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ElasticsearchAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an action that writes data to an Amazon Elasticsearch Service domain.
--
--
--
-- /See:/ 'elasticsearchAction' smart constructor.
data ElasticsearchAction = ElasticsearchAction'
  { _eaRoleARN ::
      !Text,
    _eaEndpoint :: !Text,
    _eaIndex :: !Text,
    _eaType :: !Text,
    _eaId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ElasticsearchAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eaRoleARN' - The IAM role ARN that has access to Elasticsearch.
--
-- * 'eaEndpoint' - The endpoint of your Elasticsearch domain.
--
-- * 'eaIndex' - The Elasticsearch index where you want to store your data.
--
-- * 'eaType' - The type of document you are storing.
--
-- * 'eaId' - The unique identifier for the document you are storing.
elasticsearchAction ::
  -- | 'eaRoleARN'
  Text ->
  -- | 'eaEndpoint'
  Text ->
  -- | 'eaIndex'
  Text ->
  -- | 'eaType'
  Text ->
  -- | 'eaId'
  Text ->
  ElasticsearchAction
elasticsearchAction pRoleARN_ pEndpoint_ pIndex_ pType_ pId_ =
  ElasticsearchAction'
    { _eaRoleARN = pRoleARN_,
      _eaEndpoint = pEndpoint_,
      _eaIndex = pIndex_,
      _eaType = pType_,
      _eaId = pId_
    }

-- | The IAM role ARN that has access to Elasticsearch.
eaRoleARN :: Lens' ElasticsearchAction Text
eaRoleARN = lens _eaRoleARN (\s a -> s {_eaRoleARN = a})

-- | The endpoint of your Elasticsearch domain.
eaEndpoint :: Lens' ElasticsearchAction Text
eaEndpoint = lens _eaEndpoint (\s a -> s {_eaEndpoint = a})

-- | The Elasticsearch index where you want to store your data.
eaIndex :: Lens' ElasticsearchAction Text
eaIndex = lens _eaIndex (\s a -> s {_eaIndex = a})

-- | The type of document you are storing.
eaType :: Lens' ElasticsearchAction Text
eaType = lens _eaType (\s a -> s {_eaType = a})

-- | The unique identifier for the document you are storing.
eaId :: Lens' ElasticsearchAction Text
eaId = lens _eaId (\s a -> s {_eaId = a})

instance FromJSON ElasticsearchAction where
  parseJSON =
    withObject
      "ElasticsearchAction"
      ( \x ->
          ElasticsearchAction'
            <$> (x .: "roleArn")
            <*> (x .: "endpoint")
            <*> (x .: "index")
            <*> (x .: "type")
            <*> (x .: "id")
      )

instance Hashable ElasticsearchAction

instance NFData ElasticsearchAction

instance ToJSON ElasticsearchAction where
  toJSON ElasticsearchAction' {..} =
    object
      ( catMaybes
          [ Just ("roleArn" .= _eaRoleARN),
            Just ("endpoint" .= _eaEndpoint),
            Just ("index" .= _eaIndex),
            Just ("type" .= _eaType),
            Just ("id" .= _eaId)
          ]
      )
