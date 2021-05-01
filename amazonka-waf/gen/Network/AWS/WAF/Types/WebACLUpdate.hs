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
-- Module      : Network.AWS.WAF.Types.WebACLUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.WebACLUpdate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAF.Types.ActivatedRule
import Network.AWS.WAF.Types.ChangeAction

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Specifies whether to insert a @Rule@ into or delete a @Rule@ from a
-- @WebACL@.
--
-- /See:/ 'newWebACLUpdate' smart constructor.
data WebACLUpdate = WebACLUpdate'
  { -- | Specifies whether to insert a @Rule@ into or delete a @Rule@ from a
    -- @WebACL@.
    action :: ChangeAction,
    -- | The @ActivatedRule@ object in an UpdateWebACL request specifies a @Rule@
    -- that you want to insert or delete, the priority of the @Rule@ in the
    -- @WebACL@, and the action that you want AWS WAF to take when a web
    -- request matches the @Rule@ (@ALLOW@, @BLOCK@, or @COUNT@).
    activatedRule :: ActivatedRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WebACLUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'webACLUpdate_action' - Specifies whether to insert a @Rule@ into or delete a @Rule@ from a
-- @WebACL@.
--
-- 'activatedRule', 'webACLUpdate_activatedRule' - The @ActivatedRule@ object in an UpdateWebACL request specifies a @Rule@
-- that you want to insert or delete, the priority of the @Rule@ in the
-- @WebACL@, and the action that you want AWS WAF to take when a web
-- request matches the @Rule@ (@ALLOW@, @BLOCK@, or @COUNT@).
newWebACLUpdate ::
  -- | 'action'
  ChangeAction ->
  -- | 'activatedRule'
  ActivatedRule ->
  WebACLUpdate
newWebACLUpdate pAction_ pActivatedRule_ =
  WebACLUpdate'
    { action = pAction_,
      activatedRule = pActivatedRule_
    }

-- | Specifies whether to insert a @Rule@ into or delete a @Rule@ from a
-- @WebACL@.
webACLUpdate_action :: Lens.Lens' WebACLUpdate ChangeAction
webACLUpdate_action = Lens.lens (\WebACLUpdate' {action} -> action) (\s@WebACLUpdate' {} a -> s {action = a} :: WebACLUpdate)

-- | The @ActivatedRule@ object in an UpdateWebACL request specifies a @Rule@
-- that you want to insert or delete, the priority of the @Rule@ in the
-- @WebACL@, and the action that you want AWS WAF to take when a web
-- request matches the @Rule@ (@ALLOW@, @BLOCK@, or @COUNT@).
webACLUpdate_activatedRule :: Lens.Lens' WebACLUpdate ActivatedRule
webACLUpdate_activatedRule = Lens.lens (\WebACLUpdate' {activatedRule} -> activatedRule) (\s@WebACLUpdate' {} a -> s {activatedRule = a} :: WebACLUpdate)

instance Prelude.Hashable WebACLUpdate

instance Prelude.NFData WebACLUpdate

instance Prelude.ToJSON WebACLUpdate where
  toJSON WebACLUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Action" Prelude..= action),
            Prelude.Just
              ("ActivatedRule" Prelude..= activatedRule)
          ]
      )
