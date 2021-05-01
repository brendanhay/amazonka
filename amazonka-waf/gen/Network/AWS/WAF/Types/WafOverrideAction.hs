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
-- Module      : Network.AWS.WAF.Types.WafOverrideAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.WafOverrideAction where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAF.Types.WafOverrideActionType

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- The action to take if any rule within the @RuleGroup@ matches a request.
--
-- /See:/ 'newWafOverrideAction' smart constructor.
data WafOverrideAction = WafOverrideAction'
  { -- | @COUNT@ overrides the action specified by the individual rule within a
    -- @RuleGroup@ . If set to @NONE@, the rule\'s action will take place.
    type' :: WafOverrideActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WafOverrideAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'wafOverrideAction_type' - @COUNT@ overrides the action specified by the individual rule within a
-- @RuleGroup@ . If set to @NONE@, the rule\'s action will take place.
newWafOverrideAction ::
  -- | 'type''
  WafOverrideActionType ->
  WafOverrideAction
newWafOverrideAction pType_ =
  WafOverrideAction' {type' = pType_}

-- | @COUNT@ overrides the action specified by the individual rule within a
-- @RuleGroup@ . If set to @NONE@, the rule\'s action will take place.
wafOverrideAction_type :: Lens.Lens' WafOverrideAction WafOverrideActionType
wafOverrideAction_type = Lens.lens (\WafOverrideAction' {type'} -> type') (\s@WafOverrideAction' {} a -> s {type' = a} :: WafOverrideAction)

instance Prelude.FromJSON WafOverrideAction where
  parseJSON =
    Prelude.withObject
      "WafOverrideAction"
      ( \x ->
          WafOverrideAction' Prelude.<$> (x Prelude..: "Type")
      )

instance Prelude.Hashable WafOverrideAction

instance Prelude.NFData WafOverrideAction

instance Prelude.ToJSON WafOverrideAction where
  toJSON WafOverrideAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Type" Prelude..= type')]
      )
