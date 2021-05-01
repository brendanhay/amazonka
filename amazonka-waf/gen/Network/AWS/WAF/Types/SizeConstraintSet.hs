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
-- Module      : Network.AWS.WAF.Types.SizeConstraintSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.SizeConstraintSet where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAF.Types.SizeConstraint

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- A complex type that contains @SizeConstraint@ objects, which specify the
-- parts of web requests that you want AWS WAF to inspect the size of. If a
-- @SizeConstraintSet@ contains more than one @SizeConstraint@ object, a
-- request only needs to match one constraint to be considered a match.
--
-- /See:/ 'newSizeConstraintSet' smart constructor.
data SizeConstraintSet = SizeConstraintSet'
  { -- | The name, if any, of the @SizeConstraintSet@.
    name :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a @SizeConstraintSet@. You use
    -- @SizeConstraintSetId@ to get information about a @SizeConstraintSet@
    -- (see GetSizeConstraintSet), update a @SizeConstraintSet@ (see
    -- UpdateSizeConstraintSet), insert a @SizeConstraintSet@ into a @Rule@ or
    -- delete one from a @Rule@ (see UpdateRule), and delete a
    -- @SizeConstraintSet@ from AWS WAF (see DeleteSizeConstraintSet).
    --
    -- @SizeConstraintSetId@ is returned by CreateSizeConstraintSet and by
    -- ListSizeConstraintSets.
    sizeConstraintSetId :: Prelude.Text,
    -- | Specifies the parts of web requests that you want to inspect the size
    -- of.
    sizeConstraints :: [SizeConstraint]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SizeConstraintSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'sizeConstraintSet_name' - The name, if any, of the @SizeConstraintSet@.
--
-- 'sizeConstraintSetId', 'sizeConstraintSet_sizeConstraintSetId' - A unique identifier for a @SizeConstraintSet@. You use
-- @SizeConstraintSetId@ to get information about a @SizeConstraintSet@
-- (see GetSizeConstraintSet), update a @SizeConstraintSet@ (see
-- UpdateSizeConstraintSet), insert a @SizeConstraintSet@ into a @Rule@ or
-- delete one from a @Rule@ (see UpdateRule), and delete a
-- @SizeConstraintSet@ from AWS WAF (see DeleteSizeConstraintSet).
--
-- @SizeConstraintSetId@ is returned by CreateSizeConstraintSet and by
-- ListSizeConstraintSets.
--
-- 'sizeConstraints', 'sizeConstraintSet_sizeConstraints' - Specifies the parts of web requests that you want to inspect the size
-- of.
newSizeConstraintSet ::
  -- | 'sizeConstraintSetId'
  Prelude.Text ->
  SizeConstraintSet
newSizeConstraintSet pSizeConstraintSetId_ =
  SizeConstraintSet'
    { name = Prelude.Nothing,
      sizeConstraintSetId = pSizeConstraintSetId_,
      sizeConstraints = Prelude.mempty
    }

-- | The name, if any, of the @SizeConstraintSet@.
sizeConstraintSet_name :: Lens.Lens' SizeConstraintSet (Prelude.Maybe Prelude.Text)
sizeConstraintSet_name = Lens.lens (\SizeConstraintSet' {name} -> name) (\s@SizeConstraintSet' {} a -> s {name = a} :: SizeConstraintSet)

-- | A unique identifier for a @SizeConstraintSet@. You use
-- @SizeConstraintSetId@ to get information about a @SizeConstraintSet@
-- (see GetSizeConstraintSet), update a @SizeConstraintSet@ (see
-- UpdateSizeConstraintSet), insert a @SizeConstraintSet@ into a @Rule@ or
-- delete one from a @Rule@ (see UpdateRule), and delete a
-- @SizeConstraintSet@ from AWS WAF (see DeleteSizeConstraintSet).
--
-- @SizeConstraintSetId@ is returned by CreateSizeConstraintSet and by
-- ListSizeConstraintSets.
sizeConstraintSet_sizeConstraintSetId :: Lens.Lens' SizeConstraintSet Prelude.Text
sizeConstraintSet_sizeConstraintSetId = Lens.lens (\SizeConstraintSet' {sizeConstraintSetId} -> sizeConstraintSetId) (\s@SizeConstraintSet' {} a -> s {sizeConstraintSetId = a} :: SizeConstraintSet)

-- | Specifies the parts of web requests that you want to inspect the size
-- of.
sizeConstraintSet_sizeConstraints :: Lens.Lens' SizeConstraintSet [SizeConstraint]
sizeConstraintSet_sizeConstraints = Lens.lens (\SizeConstraintSet' {sizeConstraints} -> sizeConstraints) (\s@SizeConstraintSet' {} a -> s {sizeConstraints = a} :: SizeConstraintSet) Prelude.. Prelude._Coerce

instance Prelude.FromJSON SizeConstraintSet where
  parseJSON =
    Prelude.withObject
      "SizeConstraintSet"
      ( \x ->
          SizeConstraintSet'
            Prelude.<$> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..: "SizeConstraintSetId")
            Prelude.<*> ( x Prelude..:? "SizeConstraints"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SizeConstraintSet

instance Prelude.NFData SizeConstraintSet
