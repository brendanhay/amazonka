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
-- Module      : Network.AWS.WAF.Types.XssMatchSetUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.XssMatchSetUpdate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAF.Types.ChangeAction
import Network.AWS.WAF.Types.XssMatchTuple

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Specifies the part of a web request that you want to inspect for
-- cross-site scripting attacks and indicates whether you want to add the
-- specification to an XssMatchSet or delete it from an @XssMatchSet@.
--
-- /See:/ 'newXssMatchSetUpdate' smart constructor.
data XssMatchSetUpdate = XssMatchSetUpdate'
  { -- | Specify @INSERT@ to add an XssMatchSetUpdate to an XssMatchSet. Use
    -- @DELETE@ to remove an @XssMatchSetUpdate@ from an @XssMatchSet@.
    action :: ChangeAction,
    -- | Specifies the part of a web request that you want AWS WAF to inspect for
    -- cross-site scripting attacks and, if you want AWS WAF to inspect a
    -- header, the name of the header.
    xssMatchTuple :: XssMatchTuple
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'XssMatchSetUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'xssMatchSetUpdate_action' - Specify @INSERT@ to add an XssMatchSetUpdate to an XssMatchSet. Use
-- @DELETE@ to remove an @XssMatchSetUpdate@ from an @XssMatchSet@.
--
-- 'xssMatchTuple', 'xssMatchSetUpdate_xssMatchTuple' - Specifies the part of a web request that you want AWS WAF to inspect for
-- cross-site scripting attacks and, if you want AWS WAF to inspect a
-- header, the name of the header.
newXssMatchSetUpdate ::
  -- | 'action'
  ChangeAction ->
  -- | 'xssMatchTuple'
  XssMatchTuple ->
  XssMatchSetUpdate
newXssMatchSetUpdate pAction_ pXssMatchTuple_ =
  XssMatchSetUpdate'
    { action = pAction_,
      xssMatchTuple = pXssMatchTuple_
    }

-- | Specify @INSERT@ to add an XssMatchSetUpdate to an XssMatchSet. Use
-- @DELETE@ to remove an @XssMatchSetUpdate@ from an @XssMatchSet@.
xssMatchSetUpdate_action :: Lens.Lens' XssMatchSetUpdate ChangeAction
xssMatchSetUpdate_action = Lens.lens (\XssMatchSetUpdate' {action} -> action) (\s@XssMatchSetUpdate' {} a -> s {action = a} :: XssMatchSetUpdate)

-- | Specifies the part of a web request that you want AWS WAF to inspect for
-- cross-site scripting attacks and, if you want AWS WAF to inspect a
-- header, the name of the header.
xssMatchSetUpdate_xssMatchTuple :: Lens.Lens' XssMatchSetUpdate XssMatchTuple
xssMatchSetUpdate_xssMatchTuple = Lens.lens (\XssMatchSetUpdate' {xssMatchTuple} -> xssMatchTuple) (\s@XssMatchSetUpdate' {} a -> s {xssMatchTuple = a} :: XssMatchSetUpdate)

instance Prelude.Hashable XssMatchSetUpdate

instance Prelude.NFData XssMatchSetUpdate

instance Prelude.ToJSON XssMatchSetUpdate where
  toJSON XssMatchSetUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Action" Prelude..= action),
            Prelude.Just
              ("XssMatchTuple" Prelude..= xssMatchTuple)
          ]
      )
