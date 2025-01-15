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
-- Module      : Amazonka.AMP.Types.RuleGroupsNamespaceDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AMP.Types.RuleGroupsNamespaceDescription where

import Amazonka.AMP.Types.RuleGroupsNamespaceStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents a description of the rule groups namespace.
--
-- /See:/ 'newRuleGroupsNamespaceDescription' smart constructor.
data RuleGroupsNamespaceDescription = RuleGroupsNamespaceDescription'
  { -- | The tags of this rule groups namespace.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of this rule groups namespace.
    arn :: Prelude.Text,
    -- | The time when the rule groups namespace was created.
    createdAt :: Data.POSIX,
    -- | The rule groups namespace data.
    data' :: Data.Base64,
    -- | The time when the rule groups namespace was modified.
    modifiedAt :: Data.POSIX,
    -- | The rule groups namespace name.
    name :: Prelude.Text,
    -- | The status of rule groups namespace.
    status :: RuleGroupsNamespaceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupsNamespaceDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'ruleGroupsNamespaceDescription_tags' - The tags of this rule groups namespace.
--
-- 'arn', 'ruleGroupsNamespaceDescription_arn' - The Amazon Resource Name (ARN) of this rule groups namespace.
--
-- 'createdAt', 'ruleGroupsNamespaceDescription_createdAt' - The time when the rule groups namespace was created.
--
-- 'data'', 'ruleGroupsNamespaceDescription_data' - The rule groups namespace data.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'modifiedAt', 'ruleGroupsNamespaceDescription_modifiedAt' - The time when the rule groups namespace was modified.
--
-- 'name', 'ruleGroupsNamespaceDescription_name' - The rule groups namespace name.
--
-- 'status', 'ruleGroupsNamespaceDescription_status' - The status of rule groups namespace.
newRuleGroupsNamespaceDescription ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'data''
  Prelude.ByteString ->
  -- | 'modifiedAt'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  RuleGroupsNamespaceStatus ->
  RuleGroupsNamespaceDescription
newRuleGroupsNamespaceDescription
  pArn_
  pCreatedAt_
  pData_
  pModifiedAt_
  pName_
  pStatus_ =
    RuleGroupsNamespaceDescription'
      { tags =
          Prelude.Nothing,
        arn = pArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        data' = Data._Base64 Lens.# pData_,
        modifiedAt = Data._Time Lens.# pModifiedAt_,
        name = pName_,
        status = pStatus_
      }

-- | The tags of this rule groups namespace.
ruleGroupsNamespaceDescription_tags :: Lens.Lens' RuleGroupsNamespaceDescription (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
ruleGroupsNamespaceDescription_tags = Lens.lens (\RuleGroupsNamespaceDescription' {tags} -> tags) (\s@RuleGroupsNamespaceDescription' {} a -> s {tags = a} :: RuleGroupsNamespaceDescription) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of this rule groups namespace.
ruleGroupsNamespaceDescription_arn :: Lens.Lens' RuleGroupsNamespaceDescription Prelude.Text
ruleGroupsNamespaceDescription_arn = Lens.lens (\RuleGroupsNamespaceDescription' {arn} -> arn) (\s@RuleGroupsNamespaceDescription' {} a -> s {arn = a} :: RuleGroupsNamespaceDescription)

-- | The time when the rule groups namespace was created.
ruleGroupsNamespaceDescription_createdAt :: Lens.Lens' RuleGroupsNamespaceDescription Prelude.UTCTime
ruleGroupsNamespaceDescription_createdAt = Lens.lens (\RuleGroupsNamespaceDescription' {createdAt} -> createdAt) (\s@RuleGroupsNamespaceDescription' {} a -> s {createdAt = a} :: RuleGroupsNamespaceDescription) Prelude.. Data._Time

-- | The rule groups namespace data.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
ruleGroupsNamespaceDescription_data :: Lens.Lens' RuleGroupsNamespaceDescription Prelude.ByteString
ruleGroupsNamespaceDescription_data = Lens.lens (\RuleGroupsNamespaceDescription' {data'} -> data') (\s@RuleGroupsNamespaceDescription' {} a -> s {data' = a} :: RuleGroupsNamespaceDescription) Prelude.. Data._Base64

-- | The time when the rule groups namespace was modified.
ruleGroupsNamespaceDescription_modifiedAt :: Lens.Lens' RuleGroupsNamespaceDescription Prelude.UTCTime
ruleGroupsNamespaceDescription_modifiedAt = Lens.lens (\RuleGroupsNamespaceDescription' {modifiedAt} -> modifiedAt) (\s@RuleGroupsNamespaceDescription' {} a -> s {modifiedAt = a} :: RuleGroupsNamespaceDescription) Prelude.. Data._Time

-- | The rule groups namespace name.
ruleGroupsNamespaceDescription_name :: Lens.Lens' RuleGroupsNamespaceDescription Prelude.Text
ruleGroupsNamespaceDescription_name = Lens.lens (\RuleGroupsNamespaceDescription' {name} -> name) (\s@RuleGroupsNamespaceDescription' {} a -> s {name = a} :: RuleGroupsNamespaceDescription)

-- | The status of rule groups namespace.
ruleGroupsNamespaceDescription_status :: Lens.Lens' RuleGroupsNamespaceDescription RuleGroupsNamespaceStatus
ruleGroupsNamespaceDescription_status = Lens.lens (\RuleGroupsNamespaceDescription' {status} -> status) (\s@RuleGroupsNamespaceDescription' {} a -> s {status = a} :: RuleGroupsNamespaceDescription)

instance Data.FromJSON RuleGroupsNamespaceDescription where
  parseJSON =
    Data.withObject
      "RuleGroupsNamespaceDescription"
      ( \x ->
          RuleGroupsNamespaceDescription'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdAt")
            Prelude.<*> (x Data..: "data")
            Prelude.<*> (x Data..: "modifiedAt")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "status")
      )

instance
  Prelude.Hashable
    RuleGroupsNamespaceDescription
  where
  hashWithSalt
    _salt
    RuleGroupsNamespaceDescription' {..} =
      _salt
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` createdAt
        `Prelude.hashWithSalt` data'
        `Prelude.hashWithSalt` modifiedAt
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    RuleGroupsNamespaceDescription
  where
  rnf RuleGroupsNamespaceDescription' {..} =
    Prelude.rnf tags `Prelude.seq`
      Prelude.rnf arn `Prelude.seq`
        Prelude.rnf createdAt `Prelude.seq`
          Prelude.rnf data' `Prelude.seq`
            Prelude.rnf modifiedAt `Prelude.seq`
              Prelude.rnf name `Prelude.seq`
                Prelude.rnf status
