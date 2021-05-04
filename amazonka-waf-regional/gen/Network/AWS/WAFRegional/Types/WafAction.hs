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
-- Module      : Network.AWS.WAFRegional.Types.WafAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.WafAction where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAFRegional.Types.WafActionType

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- For the action that is associated with a rule in a @WebACL@, specifies
-- the action that you want AWS WAF to perform when a web request matches
-- all of the conditions in a rule. For the default action in a @WebACL@,
-- specifies the action that you want AWS WAF to take when a web request
-- doesn\'t match all of the conditions in any of the rules in a @WebACL@.
--
-- /See:/ 'newWafAction' smart constructor.
data WafAction = WafAction'
  { -- | Specifies how you want AWS WAF to respond to requests that match the
    -- settings in a @Rule@. Valid settings include the following:
    --
    -- -   @ALLOW@: AWS WAF allows requests
    --
    -- -   @BLOCK@: AWS WAF blocks requests
    --
    -- -   @COUNT@: AWS WAF increments a counter of the requests that match all
    --     of the conditions in the rule. AWS WAF then continues to inspect the
    --     web request based on the remaining rules in the web ACL. You can\'t
    --     specify @COUNT@ for the default action for a @WebACL@.
    type' :: WafActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WafAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'wafAction_type' - Specifies how you want AWS WAF to respond to requests that match the
-- settings in a @Rule@. Valid settings include the following:
--
-- -   @ALLOW@: AWS WAF allows requests
--
-- -   @BLOCK@: AWS WAF blocks requests
--
-- -   @COUNT@: AWS WAF increments a counter of the requests that match all
--     of the conditions in the rule. AWS WAF then continues to inspect the
--     web request based on the remaining rules in the web ACL. You can\'t
--     specify @COUNT@ for the default action for a @WebACL@.
newWafAction ::
  -- | 'type''
  WafActionType ->
  WafAction
newWafAction pType_ = WafAction' {type' = pType_}

-- | Specifies how you want AWS WAF to respond to requests that match the
-- settings in a @Rule@. Valid settings include the following:
--
-- -   @ALLOW@: AWS WAF allows requests
--
-- -   @BLOCK@: AWS WAF blocks requests
--
-- -   @COUNT@: AWS WAF increments a counter of the requests that match all
--     of the conditions in the rule. AWS WAF then continues to inspect the
--     web request based on the remaining rules in the web ACL. You can\'t
--     specify @COUNT@ for the default action for a @WebACL@.
wafAction_type :: Lens.Lens' WafAction WafActionType
wafAction_type = Lens.lens (\WafAction' {type'} -> type') (\s@WafAction' {} a -> s {type' = a} :: WafAction)

instance Prelude.FromJSON WafAction where
  parseJSON =
    Prelude.withObject
      "WafAction"
      (\x -> WafAction' Prelude.<$> (x Prelude..: "Type"))

instance Prelude.Hashable WafAction

instance Prelude.NFData WafAction

instance Prelude.ToJSON WafAction where
  toJSON WafAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Type" Prelude..= type')]
      )
