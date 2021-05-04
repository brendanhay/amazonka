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
-- Module      : Network.AWS.WAF.Types.XssMatchSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.XssMatchSet where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
-- A complex type that contains @XssMatchTuple@ objects, which specify the
-- parts of web requests that you want AWS WAF to inspect for cross-site
-- scripting attacks and, if you want AWS WAF to inspect a header, the name
-- of the header. If a @XssMatchSet@ contains more than one @XssMatchTuple@
-- object, a request needs to include cross-site scripting attacks in only
-- one of the specified parts of the request to be considered a match.
--
-- /See:/ 'newXssMatchSet' smart constructor.
data XssMatchSet = XssMatchSet'
  { -- | The name, if any, of the @XssMatchSet@.
    name :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for an @XssMatchSet@. You use @XssMatchSetId@ to get
    -- information about an @XssMatchSet@ (see GetXssMatchSet), update an
    -- @XssMatchSet@ (see UpdateXssMatchSet), insert an @XssMatchSet@ into a
    -- @Rule@ or delete one from a @Rule@ (see UpdateRule), and delete an
    -- @XssMatchSet@ from AWS WAF (see DeleteXssMatchSet).
    --
    -- @XssMatchSetId@ is returned by CreateXssMatchSet and by
    -- ListXssMatchSets.
    xssMatchSetId :: Prelude.Text,
    -- | Specifies the parts of web requests that you want to inspect for
    -- cross-site scripting attacks.
    xssMatchTuples :: [XssMatchTuple]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'XssMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'xssMatchSet_name' - The name, if any, of the @XssMatchSet@.
--
-- 'xssMatchSetId', 'xssMatchSet_xssMatchSetId' - A unique identifier for an @XssMatchSet@. You use @XssMatchSetId@ to get
-- information about an @XssMatchSet@ (see GetXssMatchSet), update an
-- @XssMatchSet@ (see UpdateXssMatchSet), insert an @XssMatchSet@ into a
-- @Rule@ or delete one from a @Rule@ (see UpdateRule), and delete an
-- @XssMatchSet@ from AWS WAF (see DeleteXssMatchSet).
--
-- @XssMatchSetId@ is returned by CreateXssMatchSet and by
-- ListXssMatchSets.
--
-- 'xssMatchTuples', 'xssMatchSet_xssMatchTuples' - Specifies the parts of web requests that you want to inspect for
-- cross-site scripting attacks.
newXssMatchSet ::
  -- | 'xssMatchSetId'
  Prelude.Text ->
  XssMatchSet
newXssMatchSet pXssMatchSetId_ =
  XssMatchSet'
    { name = Prelude.Nothing,
      xssMatchSetId = pXssMatchSetId_,
      xssMatchTuples = Prelude.mempty
    }

-- | The name, if any, of the @XssMatchSet@.
xssMatchSet_name :: Lens.Lens' XssMatchSet (Prelude.Maybe Prelude.Text)
xssMatchSet_name = Lens.lens (\XssMatchSet' {name} -> name) (\s@XssMatchSet' {} a -> s {name = a} :: XssMatchSet)

-- | A unique identifier for an @XssMatchSet@. You use @XssMatchSetId@ to get
-- information about an @XssMatchSet@ (see GetXssMatchSet), update an
-- @XssMatchSet@ (see UpdateXssMatchSet), insert an @XssMatchSet@ into a
-- @Rule@ or delete one from a @Rule@ (see UpdateRule), and delete an
-- @XssMatchSet@ from AWS WAF (see DeleteXssMatchSet).
--
-- @XssMatchSetId@ is returned by CreateXssMatchSet and by
-- ListXssMatchSets.
xssMatchSet_xssMatchSetId :: Lens.Lens' XssMatchSet Prelude.Text
xssMatchSet_xssMatchSetId = Lens.lens (\XssMatchSet' {xssMatchSetId} -> xssMatchSetId) (\s@XssMatchSet' {} a -> s {xssMatchSetId = a} :: XssMatchSet)

-- | Specifies the parts of web requests that you want to inspect for
-- cross-site scripting attacks.
xssMatchSet_xssMatchTuples :: Lens.Lens' XssMatchSet [XssMatchTuple]
xssMatchSet_xssMatchTuples = Lens.lens (\XssMatchSet' {xssMatchTuples} -> xssMatchTuples) (\s@XssMatchSet' {} a -> s {xssMatchTuples = a} :: XssMatchSet) Prelude.. Prelude._Coerce

instance Prelude.FromJSON XssMatchSet where
  parseJSON =
    Prelude.withObject
      "XssMatchSet"
      ( \x ->
          XssMatchSet'
            Prelude.<$> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..: "XssMatchSetId")
            Prelude.<*> ( x Prelude..:? "XssMatchTuples"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable XssMatchSet

instance Prelude.NFData XssMatchSet
