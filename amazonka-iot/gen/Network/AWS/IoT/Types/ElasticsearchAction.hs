{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ElasticsearchAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ElasticsearchAction where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an action that writes data to an Amazon Elasticsearch Service
-- domain.
--
-- /See:/ 'newElasticsearchAction' smart constructor.
data ElasticsearchAction = ElasticsearchAction'
  { -- | The IAM role ARN that has access to Elasticsearch.
    roleArn :: Prelude.Text,
    -- | The endpoint of your Elasticsearch domain.
    endpoint :: Prelude.Text,
    -- | The Elasticsearch index where you want to store your data.
    index :: Prelude.Text,
    -- | The type of document you are storing.
    type' :: Prelude.Text,
    -- | The unique identifier for the document you are storing.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ElasticsearchAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'elasticsearchAction_roleArn' - The IAM role ARN that has access to Elasticsearch.
--
-- 'endpoint', 'elasticsearchAction_endpoint' - The endpoint of your Elasticsearch domain.
--
-- 'index', 'elasticsearchAction_index' - The Elasticsearch index where you want to store your data.
--
-- 'type'', 'elasticsearchAction_type' - The type of document you are storing.
--
-- 'id', 'elasticsearchAction_id' - The unique identifier for the document you are storing.
newElasticsearchAction ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'endpoint'
  Prelude.Text ->
  -- | 'index'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  ElasticsearchAction
newElasticsearchAction
  pRoleArn_
  pEndpoint_
  pIndex_
  pType_
  pId_ =
    ElasticsearchAction'
      { roleArn = pRoleArn_,
        endpoint = pEndpoint_,
        index = pIndex_,
        type' = pType_,
        id = pId_
      }

-- | The IAM role ARN that has access to Elasticsearch.
elasticsearchAction_roleArn :: Lens.Lens' ElasticsearchAction Prelude.Text
elasticsearchAction_roleArn = Lens.lens (\ElasticsearchAction' {roleArn} -> roleArn) (\s@ElasticsearchAction' {} a -> s {roleArn = a} :: ElasticsearchAction)

-- | The endpoint of your Elasticsearch domain.
elasticsearchAction_endpoint :: Lens.Lens' ElasticsearchAction Prelude.Text
elasticsearchAction_endpoint = Lens.lens (\ElasticsearchAction' {endpoint} -> endpoint) (\s@ElasticsearchAction' {} a -> s {endpoint = a} :: ElasticsearchAction)

-- | The Elasticsearch index where you want to store your data.
elasticsearchAction_index :: Lens.Lens' ElasticsearchAction Prelude.Text
elasticsearchAction_index = Lens.lens (\ElasticsearchAction' {index} -> index) (\s@ElasticsearchAction' {} a -> s {index = a} :: ElasticsearchAction)

-- | The type of document you are storing.
elasticsearchAction_type :: Lens.Lens' ElasticsearchAction Prelude.Text
elasticsearchAction_type = Lens.lens (\ElasticsearchAction' {type'} -> type') (\s@ElasticsearchAction' {} a -> s {type' = a} :: ElasticsearchAction)

-- | The unique identifier for the document you are storing.
elasticsearchAction_id :: Lens.Lens' ElasticsearchAction Prelude.Text
elasticsearchAction_id = Lens.lens (\ElasticsearchAction' {id} -> id) (\s@ElasticsearchAction' {} a -> s {id = a} :: ElasticsearchAction)

instance Prelude.FromJSON ElasticsearchAction where
  parseJSON =
    Prelude.withObject
      "ElasticsearchAction"
      ( \x ->
          ElasticsearchAction'
            Prelude.<$> (x Prelude..: "roleArn")
            Prelude.<*> (x Prelude..: "endpoint")
            Prelude.<*> (x Prelude..: "index")
            Prelude.<*> (x Prelude..: "type")
            Prelude.<*> (x Prelude..: "id")
      )

instance Prelude.Hashable ElasticsearchAction

instance Prelude.NFData ElasticsearchAction

instance Prelude.ToJSON ElasticsearchAction where
  toJSON ElasticsearchAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("roleArn" Prelude..= roleArn),
            Prelude.Just ("endpoint" Prelude..= endpoint),
            Prelude.Just ("index" Prelude..= index),
            Prelude.Just ("type" Prelude..= type'),
            Prelude.Just ("id" Prelude..= id)
          ]
      )
