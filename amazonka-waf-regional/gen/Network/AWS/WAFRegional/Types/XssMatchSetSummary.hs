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
-- Module      : Network.AWS.WAFRegional.Types.XssMatchSetSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.XssMatchSetSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- The @Id@ and @Name@ of an @XssMatchSet@.
--
-- /See:/ 'newXssMatchSetSummary' smart constructor.
data XssMatchSetSummary = XssMatchSetSummary'
  { -- | A unique identifier for an @XssMatchSet@. You use @XssMatchSetId@ to get
    -- information about a @XssMatchSet@ (see GetXssMatchSet), update an
    -- @XssMatchSet@ (see UpdateXssMatchSet), insert an @XssMatchSet@ into a
    -- @Rule@ or delete one from a @Rule@ (see UpdateRule), and delete an
    -- @XssMatchSet@ from AWS WAF (see DeleteXssMatchSet).
    --
    -- @XssMatchSetId@ is returned by CreateXssMatchSet and by
    -- ListXssMatchSets.
    xssMatchSetId :: Prelude.Text,
    -- | The name of the @XssMatchSet@, if any, specified by @Id@.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'XssMatchSetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'xssMatchSetId', 'xssMatchSetSummary_xssMatchSetId' - A unique identifier for an @XssMatchSet@. You use @XssMatchSetId@ to get
-- information about a @XssMatchSet@ (see GetXssMatchSet), update an
-- @XssMatchSet@ (see UpdateXssMatchSet), insert an @XssMatchSet@ into a
-- @Rule@ or delete one from a @Rule@ (see UpdateRule), and delete an
-- @XssMatchSet@ from AWS WAF (see DeleteXssMatchSet).
--
-- @XssMatchSetId@ is returned by CreateXssMatchSet and by
-- ListXssMatchSets.
--
-- 'name', 'xssMatchSetSummary_name' - The name of the @XssMatchSet@, if any, specified by @Id@.
newXssMatchSetSummary ::
  -- | 'xssMatchSetId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  XssMatchSetSummary
newXssMatchSetSummary pXssMatchSetId_ pName_ =
  XssMatchSetSummary'
    { xssMatchSetId =
        pXssMatchSetId_,
      name = pName_
    }

-- | A unique identifier for an @XssMatchSet@. You use @XssMatchSetId@ to get
-- information about a @XssMatchSet@ (see GetXssMatchSet), update an
-- @XssMatchSet@ (see UpdateXssMatchSet), insert an @XssMatchSet@ into a
-- @Rule@ or delete one from a @Rule@ (see UpdateRule), and delete an
-- @XssMatchSet@ from AWS WAF (see DeleteXssMatchSet).
--
-- @XssMatchSetId@ is returned by CreateXssMatchSet and by
-- ListXssMatchSets.
xssMatchSetSummary_xssMatchSetId :: Lens.Lens' XssMatchSetSummary Prelude.Text
xssMatchSetSummary_xssMatchSetId = Lens.lens (\XssMatchSetSummary' {xssMatchSetId} -> xssMatchSetId) (\s@XssMatchSetSummary' {} a -> s {xssMatchSetId = a} :: XssMatchSetSummary)

-- | The name of the @XssMatchSet@, if any, specified by @Id@.
xssMatchSetSummary_name :: Lens.Lens' XssMatchSetSummary Prelude.Text
xssMatchSetSummary_name = Lens.lens (\XssMatchSetSummary' {name} -> name) (\s@XssMatchSetSummary' {} a -> s {name = a} :: XssMatchSetSummary)

instance Prelude.FromJSON XssMatchSetSummary where
  parseJSON =
    Prelude.withObject
      "XssMatchSetSummary"
      ( \x ->
          XssMatchSetSummary'
            Prelude.<$> (x Prelude..: "XssMatchSetId")
            Prelude.<*> (x Prelude..: "Name")
      )

instance Prelude.Hashable XssMatchSetSummary

instance Prelude.NFData XssMatchSetSummary
