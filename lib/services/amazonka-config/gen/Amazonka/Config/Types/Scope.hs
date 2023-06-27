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
-- Module      : Amazonka.Config.Types.Scope
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.Scope where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines which resources trigger an evaluation for an Config rule. The
-- scope can include one or more resource types, a combination of a tag key
-- and value, or a combination of one resource type and one resource ID.
-- Specify a scope to constrain which resources trigger an evaluation for a
-- rule. Otherwise, evaluations for the rule are triggered when any
-- resource in your recording group changes in configuration.
--
-- /See:/ 'newScope' smart constructor.
data Scope = Scope'
  { -- | The ID of the only Amazon Web Services resource that you want to trigger
    -- an evaluation for the rule. If you specify a resource ID, you must
    -- specify one resource type for @ComplianceResourceTypes@.
    complianceResourceId :: Prelude.Maybe Prelude.Text,
    -- | The resource types of only those Amazon Web Services resources that you
    -- want to trigger an evaluation for the rule. You can only specify one
    -- type if you also specify a resource ID for @ComplianceResourceId@.
    complianceResourceTypes :: Prelude.Maybe [Prelude.Text],
    -- | The tag key that is applied to only those Amazon Web Services resources
    -- that you want to trigger an evaluation for the rule.
    tagKey :: Prelude.Maybe Prelude.Text,
    -- | The tag value applied to only those Amazon Web Services resources that
    -- you want to trigger an evaluation for the rule. If you specify a value
    -- for @TagValue@, you must also specify a value for @TagKey@.
    tagValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Scope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'complianceResourceId', 'scope_complianceResourceId' - The ID of the only Amazon Web Services resource that you want to trigger
-- an evaluation for the rule. If you specify a resource ID, you must
-- specify one resource type for @ComplianceResourceTypes@.
--
-- 'complianceResourceTypes', 'scope_complianceResourceTypes' - The resource types of only those Amazon Web Services resources that you
-- want to trigger an evaluation for the rule. You can only specify one
-- type if you also specify a resource ID for @ComplianceResourceId@.
--
-- 'tagKey', 'scope_tagKey' - The tag key that is applied to only those Amazon Web Services resources
-- that you want to trigger an evaluation for the rule.
--
-- 'tagValue', 'scope_tagValue' - The tag value applied to only those Amazon Web Services resources that
-- you want to trigger an evaluation for the rule. If you specify a value
-- for @TagValue@, you must also specify a value for @TagKey@.
newScope ::
  Scope
newScope =
  Scope'
    { complianceResourceId = Prelude.Nothing,
      complianceResourceTypes = Prelude.Nothing,
      tagKey = Prelude.Nothing,
      tagValue = Prelude.Nothing
    }

-- | The ID of the only Amazon Web Services resource that you want to trigger
-- an evaluation for the rule. If you specify a resource ID, you must
-- specify one resource type for @ComplianceResourceTypes@.
scope_complianceResourceId :: Lens.Lens' Scope (Prelude.Maybe Prelude.Text)
scope_complianceResourceId = Lens.lens (\Scope' {complianceResourceId} -> complianceResourceId) (\s@Scope' {} a -> s {complianceResourceId = a} :: Scope)

-- | The resource types of only those Amazon Web Services resources that you
-- want to trigger an evaluation for the rule. You can only specify one
-- type if you also specify a resource ID for @ComplianceResourceId@.
scope_complianceResourceTypes :: Lens.Lens' Scope (Prelude.Maybe [Prelude.Text])
scope_complianceResourceTypes = Lens.lens (\Scope' {complianceResourceTypes} -> complianceResourceTypes) (\s@Scope' {} a -> s {complianceResourceTypes = a} :: Scope) Prelude.. Lens.mapping Lens.coerced

-- | The tag key that is applied to only those Amazon Web Services resources
-- that you want to trigger an evaluation for the rule.
scope_tagKey :: Lens.Lens' Scope (Prelude.Maybe Prelude.Text)
scope_tagKey = Lens.lens (\Scope' {tagKey} -> tagKey) (\s@Scope' {} a -> s {tagKey = a} :: Scope)

-- | The tag value applied to only those Amazon Web Services resources that
-- you want to trigger an evaluation for the rule. If you specify a value
-- for @TagValue@, you must also specify a value for @TagKey@.
scope_tagValue :: Lens.Lens' Scope (Prelude.Maybe Prelude.Text)
scope_tagValue = Lens.lens (\Scope' {tagValue} -> tagValue) (\s@Scope' {} a -> s {tagValue = a} :: Scope)

instance Data.FromJSON Scope where
  parseJSON =
    Data.withObject
      "Scope"
      ( \x ->
          Scope'
            Prelude.<$> (x Data..:? "ComplianceResourceId")
            Prelude.<*> ( x
                            Data..:? "ComplianceResourceTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "TagKey")
            Prelude.<*> (x Data..:? "TagValue")
      )

instance Prelude.Hashable Scope where
  hashWithSalt _salt Scope' {..} =
    _salt
      `Prelude.hashWithSalt` complianceResourceId
      `Prelude.hashWithSalt` complianceResourceTypes
      `Prelude.hashWithSalt` tagKey
      `Prelude.hashWithSalt` tagValue

instance Prelude.NFData Scope where
  rnf Scope' {..} =
    Prelude.rnf complianceResourceId
      `Prelude.seq` Prelude.rnf complianceResourceTypes
      `Prelude.seq` Prelude.rnf tagKey
      `Prelude.seq` Prelude.rnf tagValue

instance Data.ToJSON Scope where
  toJSON Scope' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ComplianceResourceId" Data..=)
              Prelude.<$> complianceResourceId,
            ("ComplianceResourceTypes" Data..=)
              Prelude.<$> complianceResourceTypes,
            ("TagKey" Data..=) Prelude.<$> tagKey,
            ("TagValue" Data..=) Prelude.<$> tagValue
          ]
      )
