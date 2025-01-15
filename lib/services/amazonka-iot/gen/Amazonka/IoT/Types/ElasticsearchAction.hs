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
-- Module      : Amazonka.IoT.Types.ElasticsearchAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ElasticsearchAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an action that writes data to an Amazon OpenSearch Service
-- domain.
--
-- The @Elasticsearch@ action can only be used by existing rule actions. To
-- create a new rule action or to update an existing rule action, use the
-- @OpenSearch@ rule action instead. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/apireference/API_OpenSearchAction.html OpenSearchAction>.
--
-- /See:/ 'newElasticsearchAction' smart constructor.
data ElasticsearchAction = ElasticsearchAction'
  { -- | The IAM role ARN that has access to OpenSearch.
    roleArn :: Prelude.Text,
    -- | The endpoint of your OpenSearch domain.
    endpoint :: Prelude.Text,
    -- | The index where you want to store your data.
    index :: Prelude.Text,
    -- | The type of document you are storing.
    type' :: Prelude.Text,
    -- | The unique identifier for the document you are storing.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ElasticsearchAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'elasticsearchAction_roleArn' - The IAM role ARN that has access to OpenSearch.
--
-- 'endpoint', 'elasticsearchAction_endpoint' - The endpoint of your OpenSearch domain.
--
-- 'index', 'elasticsearchAction_index' - The index where you want to store your data.
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

-- | The IAM role ARN that has access to OpenSearch.
elasticsearchAction_roleArn :: Lens.Lens' ElasticsearchAction Prelude.Text
elasticsearchAction_roleArn = Lens.lens (\ElasticsearchAction' {roleArn} -> roleArn) (\s@ElasticsearchAction' {} a -> s {roleArn = a} :: ElasticsearchAction)

-- | The endpoint of your OpenSearch domain.
elasticsearchAction_endpoint :: Lens.Lens' ElasticsearchAction Prelude.Text
elasticsearchAction_endpoint = Lens.lens (\ElasticsearchAction' {endpoint} -> endpoint) (\s@ElasticsearchAction' {} a -> s {endpoint = a} :: ElasticsearchAction)

-- | The index where you want to store your data.
elasticsearchAction_index :: Lens.Lens' ElasticsearchAction Prelude.Text
elasticsearchAction_index = Lens.lens (\ElasticsearchAction' {index} -> index) (\s@ElasticsearchAction' {} a -> s {index = a} :: ElasticsearchAction)

-- | The type of document you are storing.
elasticsearchAction_type :: Lens.Lens' ElasticsearchAction Prelude.Text
elasticsearchAction_type = Lens.lens (\ElasticsearchAction' {type'} -> type') (\s@ElasticsearchAction' {} a -> s {type' = a} :: ElasticsearchAction)

-- | The unique identifier for the document you are storing.
elasticsearchAction_id :: Lens.Lens' ElasticsearchAction Prelude.Text
elasticsearchAction_id = Lens.lens (\ElasticsearchAction' {id} -> id) (\s@ElasticsearchAction' {} a -> s {id = a} :: ElasticsearchAction)

instance Data.FromJSON ElasticsearchAction where
  parseJSON =
    Data.withObject
      "ElasticsearchAction"
      ( \x ->
          ElasticsearchAction'
            Prelude.<$> (x Data..: "roleArn")
            Prelude.<*> (x Data..: "endpoint")
            Prelude.<*> (x Data..: "index")
            Prelude.<*> (x Data..: "type")
            Prelude.<*> (x Data..: "id")
      )

instance Prelude.Hashable ElasticsearchAction where
  hashWithSalt _salt ElasticsearchAction' {..} =
    _salt
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` index
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` id

instance Prelude.NFData ElasticsearchAction where
  rnf ElasticsearchAction' {..} =
    Prelude.rnf roleArn `Prelude.seq`
      Prelude.rnf endpoint `Prelude.seq`
        Prelude.rnf index `Prelude.seq`
          Prelude.rnf type' `Prelude.seq`
            Prelude.rnf id

instance Data.ToJSON ElasticsearchAction where
  toJSON ElasticsearchAction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("roleArn" Data..= roleArn),
            Prelude.Just ("endpoint" Data..= endpoint),
            Prelude.Just ("index" Data..= index),
            Prelude.Just ("type" Data..= type'),
            Prelude.Just ("id" Data..= id)
          ]
      )
