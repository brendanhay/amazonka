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
-- Module      : Amazonka.IoT.Types.OpenSearchAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.OpenSearchAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an action that writes data to an Amazon OpenSearch Service
-- domain.
--
-- /See:/ 'newOpenSearchAction' smart constructor.
data OpenSearchAction = OpenSearchAction'
  { -- | The IAM role ARN that has access to OpenSearch.
    roleArn :: Prelude.Text,
    -- | The endpoint of your OpenSearch domain.
    endpoint :: Prelude.Text,
    -- | The OpenSearch index where you want to store your data.
    index :: Prelude.Text,
    -- | The type of document you are storing.
    type' :: Prelude.Text,
    -- | The unique identifier for the document you are storing.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OpenSearchAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'openSearchAction_roleArn' - The IAM role ARN that has access to OpenSearch.
--
-- 'endpoint', 'openSearchAction_endpoint' - The endpoint of your OpenSearch domain.
--
-- 'index', 'openSearchAction_index' - The OpenSearch index where you want to store your data.
--
-- 'type'', 'openSearchAction_type' - The type of document you are storing.
--
-- 'id', 'openSearchAction_id' - The unique identifier for the document you are storing.
newOpenSearchAction ::
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
  OpenSearchAction
newOpenSearchAction
  pRoleArn_
  pEndpoint_
  pIndex_
  pType_
  pId_ =
    OpenSearchAction'
      { roleArn = pRoleArn_,
        endpoint = pEndpoint_,
        index = pIndex_,
        type' = pType_,
        id = pId_
      }

-- | The IAM role ARN that has access to OpenSearch.
openSearchAction_roleArn :: Lens.Lens' OpenSearchAction Prelude.Text
openSearchAction_roleArn = Lens.lens (\OpenSearchAction' {roleArn} -> roleArn) (\s@OpenSearchAction' {} a -> s {roleArn = a} :: OpenSearchAction)

-- | The endpoint of your OpenSearch domain.
openSearchAction_endpoint :: Lens.Lens' OpenSearchAction Prelude.Text
openSearchAction_endpoint = Lens.lens (\OpenSearchAction' {endpoint} -> endpoint) (\s@OpenSearchAction' {} a -> s {endpoint = a} :: OpenSearchAction)

-- | The OpenSearch index where you want to store your data.
openSearchAction_index :: Lens.Lens' OpenSearchAction Prelude.Text
openSearchAction_index = Lens.lens (\OpenSearchAction' {index} -> index) (\s@OpenSearchAction' {} a -> s {index = a} :: OpenSearchAction)

-- | The type of document you are storing.
openSearchAction_type :: Lens.Lens' OpenSearchAction Prelude.Text
openSearchAction_type = Lens.lens (\OpenSearchAction' {type'} -> type') (\s@OpenSearchAction' {} a -> s {type' = a} :: OpenSearchAction)

-- | The unique identifier for the document you are storing.
openSearchAction_id :: Lens.Lens' OpenSearchAction Prelude.Text
openSearchAction_id = Lens.lens (\OpenSearchAction' {id} -> id) (\s@OpenSearchAction' {} a -> s {id = a} :: OpenSearchAction)

instance Core.FromJSON OpenSearchAction where
  parseJSON =
    Core.withObject
      "OpenSearchAction"
      ( \x ->
          OpenSearchAction'
            Prelude.<$> (x Core..: "roleArn")
            Prelude.<*> (x Core..: "endpoint")
            Prelude.<*> (x Core..: "index")
            Prelude.<*> (x Core..: "type")
            Prelude.<*> (x Core..: "id")
      )

instance Prelude.Hashable OpenSearchAction where
  hashWithSalt _salt OpenSearchAction' {..} =
    _salt `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` index
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` id

instance Prelude.NFData OpenSearchAction where
  rnf OpenSearchAction' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf index
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf id

instance Core.ToJSON OpenSearchAction where
  toJSON OpenSearchAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("roleArn" Core..= roleArn),
            Prelude.Just ("endpoint" Core..= endpoint),
            Prelude.Just ("index" Core..= index),
            Prelude.Just ("type" Core..= type'),
            Prelude.Just ("id" Core..= id)
          ]
      )
