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
-- Module      : Network.AWS.Config.Types.StoredQueryMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.StoredQueryMetadata where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns details of a specific query.
--
-- /See:/ 'newStoredQueryMetadata' smart constructor.
data StoredQueryMetadata = StoredQueryMetadata'
  { -- | A unique description for the query.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the query.
    queryId :: Prelude.Text,
    -- | Amazon Resource Name (ARN) of the query. For example,
    -- arn:partition:service:region:account-id:resource-type\/resource-name\/resource-id.
    queryArn :: Prelude.Text,
    -- | The name of the query.
    queryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StoredQueryMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'storedQueryMetadata_description' - A unique description for the query.
--
-- 'queryId', 'storedQueryMetadata_queryId' - The ID of the query.
--
-- 'queryArn', 'storedQueryMetadata_queryArn' - Amazon Resource Name (ARN) of the query. For example,
-- arn:partition:service:region:account-id:resource-type\/resource-name\/resource-id.
--
-- 'queryName', 'storedQueryMetadata_queryName' - The name of the query.
newStoredQueryMetadata ::
  -- | 'queryId'
  Prelude.Text ->
  -- | 'queryArn'
  Prelude.Text ->
  -- | 'queryName'
  Prelude.Text ->
  StoredQueryMetadata
newStoredQueryMetadata
  pQueryId_
  pQueryArn_
  pQueryName_ =
    StoredQueryMetadata'
      { description = Prelude.Nothing,
        queryId = pQueryId_,
        queryArn = pQueryArn_,
        queryName = pQueryName_
      }

-- | A unique description for the query.
storedQueryMetadata_description :: Lens.Lens' StoredQueryMetadata (Prelude.Maybe Prelude.Text)
storedQueryMetadata_description = Lens.lens (\StoredQueryMetadata' {description} -> description) (\s@StoredQueryMetadata' {} a -> s {description = a} :: StoredQueryMetadata)

-- | The ID of the query.
storedQueryMetadata_queryId :: Lens.Lens' StoredQueryMetadata Prelude.Text
storedQueryMetadata_queryId = Lens.lens (\StoredQueryMetadata' {queryId} -> queryId) (\s@StoredQueryMetadata' {} a -> s {queryId = a} :: StoredQueryMetadata)

-- | Amazon Resource Name (ARN) of the query. For example,
-- arn:partition:service:region:account-id:resource-type\/resource-name\/resource-id.
storedQueryMetadata_queryArn :: Lens.Lens' StoredQueryMetadata Prelude.Text
storedQueryMetadata_queryArn = Lens.lens (\StoredQueryMetadata' {queryArn} -> queryArn) (\s@StoredQueryMetadata' {} a -> s {queryArn = a} :: StoredQueryMetadata)

-- | The name of the query.
storedQueryMetadata_queryName :: Lens.Lens' StoredQueryMetadata Prelude.Text
storedQueryMetadata_queryName = Lens.lens (\StoredQueryMetadata' {queryName} -> queryName) (\s@StoredQueryMetadata' {} a -> s {queryName = a} :: StoredQueryMetadata)

instance Prelude.FromJSON StoredQueryMetadata where
  parseJSON =
    Prelude.withObject
      "StoredQueryMetadata"
      ( \x ->
          StoredQueryMetadata'
            Prelude.<$> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..: "QueryId")
            Prelude.<*> (x Prelude..: "QueryArn")
            Prelude.<*> (x Prelude..: "QueryName")
      )

instance Prelude.Hashable StoredQueryMetadata

instance Prelude.NFData StoredQueryMetadata
