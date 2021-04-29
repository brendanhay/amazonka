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
-- Module      : Network.AWS.Config.Types.Scope
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.Scope where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Defines which resources trigger an evaluation for an AWS Config rule.
-- The scope can include one or more resource types, a combination of a tag
-- key and value, or a combination of one resource type and one resource
-- ID. Specify a scope to constrain which resources trigger an evaluation
-- for a rule. Otherwise, evaluations for the rule are triggered when any
-- resource in your recording group changes in configuration.
--
-- /See:/ 'newScope' smart constructor.
data Scope = Scope'
  { -- | The tag value applied to only those AWS resources that you want to
    -- trigger an evaluation for the rule. If you specify a value for
    -- @TagValue@, you must also specify a value for @TagKey@.
    tagValue :: Prelude.Maybe Prelude.Text,
    -- | The tag key that is applied to only those AWS resources that you want to
    -- trigger an evaluation for the rule.
    tagKey :: Prelude.Maybe Prelude.Text,
    -- | The ID of the only AWS resource that you want to trigger an evaluation
    -- for the rule. If you specify a resource ID, you must specify one
    -- resource type for @ComplianceResourceTypes@.
    complianceResourceId :: Prelude.Maybe Prelude.Text,
    -- | The resource types of only those AWS resources that you want to trigger
    -- an evaluation for the rule. You can only specify one type if you also
    -- specify a resource ID for @ComplianceResourceId@.
    complianceResourceTypes :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Scope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagValue', 'scope_tagValue' - The tag value applied to only those AWS resources that you want to
-- trigger an evaluation for the rule. If you specify a value for
-- @TagValue@, you must also specify a value for @TagKey@.
--
-- 'tagKey', 'scope_tagKey' - The tag key that is applied to only those AWS resources that you want to
-- trigger an evaluation for the rule.
--
-- 'complianceResourceId', 'scope_complianceResourceId' - The ID of the only AWS resource that you want to trigger an evaluation
-- for the rule. If you specify a resource ID, you must specify one
-- resource type for @ComplianceResourceTypes@.
--
-- 'complianceResourceTypes', 'scope_complianceResourceTypes' - The resource types of only those AWS resources that you want to trigger
-- an evaluation for the rule. You can only specify one type if you also
-- specify a resource ID for @ComplianceResourceId@.
newScope ::
  Scope
newScope =
  Scope'
    { tagValue = Prelude.Nothing,
      tagKey = Prelude.Nothing,
      complianceResourceId = Prelude.Nothing,
      complianceResourceTypes = Prelude.Nothing
    }

-- | The tag value applied to only those AWS resources that you want to
-- trigger an evaluation for the rule. If you specify a value for
-- @TagValue@, you must also specify a value for @TagKey@.
scope_tagValue :: Lens.Lens' Scope (Prelude.Maybe Prelude.Text)
scope_tagValue = Lens.lens (\Scope' {tagValue} -> tagValue) (\s@Scope' {} a -> s {tagValue = a} :: Scope)

-- | The tag key that is applied to only those AWS resources that you want to
-- trigger an evaluation for the rule.
scope_tagKey :: Lens.Lens' Scope (Prelude.Maybe Prelude.Text)
scope_tagKey = Lens.lens (\Scope' {tagKey} -> tagKey) (\s@Scope' {} a -> s {tagKey = a} :: Scope)

-- | The ID of the only AWS resource that you want to trigger an evaluation
-- for the rule. If you specify a resource ID, you must specify one
-- resource type for @ComplianceResourceTypes@.
scope_complianceResourceId :: Lens.Lens' Scope (Prelude.Maybe Prelude.Text)
scope_complianceResourceId = Lens.lens (\Scope' {complianceResourceId} -> complianceResourceId) (\s@Scope' {} a -> s {complianceResourceId = a} :: Scope)

-- | The resource types of only those AWS resources that you want to trigger
-- an evaluation for the rule. You can only specify one type if you also
-- specify a resource ID for @ComplianceResourceId@.
scope_complianceResourceTypes :: Lens.Lens' Scope (Prelude.Maybe [Prelude.Text])
scope_complianceResourceTypes = Lens.lens (\Scope' {complianceResourceTypes} -> complianceResourceTypes) (\s@Scope' {} a -> s {complianceResourceTypes = a} :: Scope) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON Scope where
  parseJSON =
    Prelude.withObject
      "Scope"
      ( \x ->
          Scope'
            Prelude.<$> (x Prelude..:? "TagValue")
            Prelude.<*> (x Prelude..:? "TagKey")
            Prelude.<*> (x Prelude..:? "ComplianceResourceId")
            Prelude.<*> ( x Prelude..:? "ComplianceResourceTypes"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable Scope

instance Prelude.NFData Scope

instance Prelude.ToJSON Scope where
  toJSON Scope' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("TagValue" Prelude..=) Prelude.<$> tagValue,
            ("TagKey" Prelude..=) Prelude.<$> tagKey,
            ("ComplianceResourceId" Prelude..=)
              Prelude.<$> complianceResourceId,
            ("ComplianceResourceTypes" Prelude..=)
              Prelude.<$> complianceResourceTypes
          ]
      )
