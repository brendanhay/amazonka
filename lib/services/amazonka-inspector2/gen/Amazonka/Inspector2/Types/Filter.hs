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
-- Module      : Amazonka.Inspector2.Types.Filter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Filter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.FilterAction
import Amazonka.Inspector2.Types.FilterCriteria
import qualified Amazonka.Prelude as Prelude

-- | Details about a filter.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | A description of the filter.
    description :: Prelude.Maybe Prelude.Text,
    -- | The reason for the filter.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The tags attached to the filter.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The action that is to be applied to the findings that match the filter.
    action :: FilterAction,
    -- | The Amazon Resource Number (ARN) associated with this filter.
    arn :: Prelude.Text,
    -- | The date and time this filter was created at.
    createdAt :: Data.POSIX,
    -- | Details on the filter criteria associated with this filter.
    criteria :: FilterCriteria,
    -- | The name of the filter.
    name :: Prelude.Text,
    -- | The Amazon Web Services account ID of the account that created the
    -- filter.
    ownerId :: Prelude.Text,
    -- | The date and time the filter was last updated at.
    updatedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'filter_description' - A description of the filter.
--
-- 'reason', 'filter_reason' - The reason for the filter.
--
-- 'tags', 'filter_tags' - The tags attached to the filter.
--
-- 'action', 'filter_action' - The action that is to be applied to the findings that match the filter.
--
-- 'arn', 'filter_arn' - The Amazon Resource Number (ARN) associated with this filter.
--
-- 'createdAt', 'filter_createdAt' - The date and time this filter was created at.
--
-- 'criteria', 'filter_criteria' - Details on the filter criteria associated with this filter.
--
-- 'name', 'filter_name' - The name of the filter.
--
-- 'ownerId', 'filter_ownerId' - The Amazon Web Services account ID of the account that created the
-- filter.
--
-- 'updatedAt', 'filter_updatedAt' - The date and time the filter was last updated at.
newFilter ::
  -- | 'action'
  FilterAction ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'criteria'
  FilterCriteria ->
  -- | 'name'
  Prelude.Text ->
  -- | 'ownerId'
  Prelude.Text ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  Filter
newFilter
  pAction_
  pArn_
  pCreatedAt_
  pCriteria_
  pName_
  pOwnerId_
  pUpdatedAt_ =
    Filter'
      { description = Prelude.Nothing,
        reason = Prelude.Nothing,
        tags = Prelude.Nothing,
        action = pAction_,
        arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        criteria = pCriteria_,
        name = pName_,
        ownerId = pOwnerId_,
        updatedAt = Data._Time Lens.# pUpdatedAt_
      }

-- | A description of the filter.
filter_description :: Lens.Lens' Filter (Prelude.Maybe Prelude.Text)
filter_description = Lens.lens (\Filter' {description} -> description) (\s@Filter' {} a -> s {description = a} :: Filter)

-- | The reason for the filter.
filter_reason :: Lens.Lens' Filter (Prelude.Maybe Prelude.Text)
filter_reason = Lens.lens (\Filter' {reason} -> reason) (\s@Filter' {} a -> s {reason = a} :: Filter)

-- | The tags attached to the filter.
filter_tags :: Lens.Lens' Filter (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
filter_tags = Lens.lens (\Filter' {tags} -> tags) (\s@Filter' {} a -> s {tags = a} :: Filter) Prelude.. Lens.mapping Lens.coerced

-- | The action that is to be applied to the findings that match the filter.
filter_action :: Lens.Lens' Filter FilterAction
filter_action = Lens.lens (\Filter' {action} -> action) (\s@Filter' {} a -> s {action = a} :: Filter)

-- | The Amazon Resource Number (ARN) associated with this filter.
filter_arn :: Lens.Lens' Filter Prelude.Text
filter_arn = Lens.lens (\Filter' {arn} -> arn) (\s@Filter' {} a -> s {arn = a} :: Filter)

-- | The date and time this filter was created at.
filter_createdAt :: Lens.Lens' Filter Prelude.UTCTime
filter_createdAt = Lens.lens (\Filter' {createdAt} -> createdAt) (\s@Filter' {} a -> s {createdAt = a} :: Filter) Prelude.. Data._Time

-- | Details on the filter criteria associated with this filter.
filter_criteria :: Lens.Lens' Filter FilterCriteria
filter_criteria = Lens.lens (\Filter' {criteria} -> criteria) (\s@Filter' {} a -> s {criteria = a} :: Filter)

-- | The name of the filter.
filter_name :: Lens.Lens' Filter Prelude.Text
filter_name = Lens.lens (\Filter' {name} -> name) (\s@Filter' {} a -> s {name = a} :: Filter)

-- | The Amazon Web Services account ID of the account that created the
-- filter.
filter_ownerId :: Lens.Lens' Filter Prelude.Text
filter_ownerId = Lens.lens (\Filter' {ownerId} -> ownerId) (\s@Filter' {} a -> s {ownerId = a} :: Filter)

-- | The date and time the filter was last updated at.
filter_updatedAt :: Lens.Lens' Filter Prelude.UTCTime
filter_updatedAt = Lens.lens (\Filter' {updatedAt} -> updatedAt) (\s@Filter' {} a -> s {updatedAt = a} :: Filter) Prelude.. Data._Time

instance Data.FromJSON Filter where
  parseJSON =
    Data.withObject
      "Filter"
      ( \x ->
          Filter'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "reason")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "action")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "criteria")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "ownerId")
            Prelude.<*> (x Data..: "updatedAt")
      )

instance Prelude.Hashable Filter where
  hashWithSalt _salt Filter' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` criteria
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData Filter where
  rnf Filter' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf criteria
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf updatedAt
