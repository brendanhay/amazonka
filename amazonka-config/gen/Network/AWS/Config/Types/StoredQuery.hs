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
-- Module      : Network.AWS.Config.Types.StoredQuery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.StoredQuery where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides the details of a stored query.
--
-- /See:/ 'newStoredQuery' smart constructor.
data StoredQuery = StoredQuery'
  { -- | Amazon Resource Name (ARN) of the query. For example,
    -- arn:partition:service:region:account-id:resource-type\/resource-name\/resource-id.
    queryArn :: Core.Maybe Core.Text,
    -- | The ID of the query.
    queryId :: Core.Maybe Core.Text,
    -- | A unique description for the query.
    description :: Core.Maybe Core.Text,
    -- | The expression of the query. For example,
    -- @SELECT resourceId, resourceType, supplementaryConfiguration.BucketVersioningConfiguration.status WHERE resourceType = \'AWS::S3::Bucket\' AND supplementaryConfiguration.BucketVersioningConfiguration.status = \'Off\'.@
    expression :: Core.Maybe Core.Text,
    -- | The name of the query.
    queryName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StoredQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryArn', 'storedQuery_queryArn' - Amazon Resource Name (ARN) of the query. For example,
-- arn:partition:service:region:account-id:resource-type\/resource-name\/resource-id.
--
-- 'queryId', 'storedQuery_queryId' - The ID of the query.
--
-- 'description', 'storedQuery_description' - A unique description for the query.
--
-- 'expression', 'storedQuery_expression' - The expression of the query. For example,
-- @SELECT resourceId, resourceType, supplementaryConfiguration.BucketVersioningConfiguration.status WHERE resourceType = \'AWS::S3::Bucket\' AND supplementaryConfiguration.BucketVersioningConfiguration.status = \'Off\'.@
--
-- 'queryName', 'storedQuery_queryName' - The name of the query.
newStoredQuery ::
  -- | 'queryName'
  Core.Text ->
  StoredQuery
newStoredQuery pQueryName_ =
  StoredQuery'
    { queryArn = Core.Nothing,
      queryId = Core.Nothing,
      description = Core.Nothing,
      expression = Core.Nothing,
      queryName = pQueryName_
    }

-- | Amazon Resource Name (ARN) of the query. For example,
-- arn:partition:service:region:account-id:resource-type\/resource-name\/resource-id.
storedQuery_queryArn :: Lens.Lens' StoredQuery (Core.Maybe Core.Text)
storedQuery_queryArn = Lens.lens (\StoredQuery' {queryArn} -> queryArn) (\s@StoredQuery' {} a -> s {queryArn = a} :: StoredQuery)

-- | The ID of the query.
storedQuery_queryId :: Lens.Lens' StoredQuery (Core.Maybe Core.Text)
storedQuery_queryId = Lens.lens (\StoredQuery' {queryId} -> queryId) (\s@StoredQuery' {} a -> s {queryId = a} :: StoredQuery)

-- | A unique description for the query.
storedQuery_description :: Lens.Lens' StoredQuery (Core.Maybe Core.Text)
storedQuery_description = Lens.lens (\StoredQuery' {description} -> description) (\s@StoredQuery' {} a -> s {description = a} :: StoredQuery)

-- | The expression of the query. For example,
-- @SELECT resourceId, resourceType, supplementaryConfiguration.BucketVersioningConfiguration.status WHERE resourceType = \'AWS::S3::Bucket\' AND supplementaryConfiguration.BucketVersioningConfiguration.status = \'Off\'.@
storedQuery_expression :: Lens.Lens' StoredQuery (Core.Maybe Core.Text)
storedQuery_expression = Lens.lens (\StoredQuery' {expression} -> expression) (\s@StoredQuery' {} a -> s {expression = a} :: StoredQuery)

-- | The name of the query.
storedQuery_queryName :: Lens.Lens' StoredQuery Core.Text
storedQuery_queryName = Lens.lens (\StoredQuery' {queryName} -> queryName) (\s@StoredQuery' {} a -> s {queryName = a} :: StoredQuery)

instance Core.FromJSON StoredQuery where
  parseJSON =
    Core.withObject
      "StoredQuery"
      ( \x ->
          StoredQuery'
            Core.<$> (x Core..:? "QueryArn")
            Core.<*> (x Core..:? "QueryId")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "Expression")
            Core.<*> (x Core..: "QueryName")
      )

instance Core.Hashable StoredQuery

instance Core.NFData StoredQuery

instance Core.ToJSON StoredQuery where
  toJSON StoredQuery' {..} =
    Core.object
      ( Core.catMaybes
          [ ("QueryArn" Core..=) Core.<$> queryArn,
            ("QueryId" Core..=) Core.<$> queryId,
            ("Description" Core..=) Core.<$> description,
            ("Expression" Core..=) Core.<$> expression,
            Core.Just ("QueryName" Core..= queryName)
          ]
      )
