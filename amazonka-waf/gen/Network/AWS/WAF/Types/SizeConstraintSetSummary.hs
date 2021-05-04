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
-- Module      : Network.AWS.WAF.Types.SizeConstraintSetSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.SizeConstraintSetSummary where

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
-- The @Id@ and @Name@ of a @SizeConstraintSet@.
--
-- /See:/ 'newSizeConstraintSetSummary' smart constructor.
data SizeConstraintSetSummary = SizeConstraintSetSummary'
  { -- | A unique identifier for a @SizeConstraintSet@. You use
    -- @SizeConstraintSetId@ to get information about a @SizeConstraintSet@
    -- (see GetSizeConstraintSet), update a @SizeConstraintSet@ (see
    -- UpdateSizeConstraintSet), insert a @SizeConstraintSet@ into a @Rule@ or
    -- delete one from a @Rule@ (see UpdateRule), and delete a
    -- @SizeConstraintSet@ from AWS WAF (see DeleteSizeConstraintSet).
    --
    -- @SizeConstraintSetId@ is returned by CreateSizeConstraintSet and by
    -- ListSizeConstraintSets.
    sizeConstraintSetId :: Prelude.Text,
    -- | The name of the @SizeConstraintSet@, if any.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SizeConstraintSetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeConstraintSetId', 'sizeConstraintSetSummary_sizeConstraintSetId' - A unique identifier for a @SizeConstraintSet@. You use
-- @SizeConstraintSetId@ to get information about a @SizeConstraintSet@
-- (see GetSizeConstraintSet), update a @SizeConstraintSet@ (see
-- UpdateSizeConstraintSet), insert a @SizeConstraintSet@ into a @Rule@ or
-- delete one from a @Rule@ (see UpdateRule), and delete a
-- @SizeConstraintSet@ from AWS WAF (see DeleteSizeConstraintSet).
--
-- @SizeConstraintSetId@ is returned by CreateSizeConstraintSet and by
-- ListSizeConstraintSets.
--
-- 'name', 'sizeConstraintSetSummary_name' - The name of the @SizeConstraintSet@, if any.
newSizeConstraintSetSummary ::
  -- | 'sizeConstraintSetId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  SizeConstraintSetSummary
newSizeConstraintSetSummary
  pSizeConstraintSetId_
  pName_ =
    SizeConstraintSetSummary'
      { sizeConstraintSetId =
          pSizeConstraintSetId_,
        name = pName_
      }

-- | A unique identifier for a @SizeConstraintSet@. You use
-- @SizeConstraintSetId@ to get information about a @SizeConstraintSet@
-- (see GetSizeConstraintSet), update a @SizeConstraintSet@ (see
-- UpdateSizeConstraintSet), insert a @SizeConstraintSet@ into a @Rule@ or
-- delete one from a @Rule@ (see UpdateRule), and delete a
-- @SizeConstraintSet@ from AWS WAF (see DeleteSizeConstraintSet).
--
-- @SizeConstraintSetId@ is returned by CreateSizeConstraintSet and by
-- ListSizeConstraintSets.
sizeConstraintSetSummary_sizeConstraintSetId :: Lens.Lens' SizeConstraintSetSummary Prelude.Text
sizeConstraintSetSummary_sizeConstraintSetId = Lens.lens (\SizeConstraintSetSummary' {sizeConstraintSetId} -> sizeConstraintSetId) (\s@SizeConstraintSetSummary' {} a -> s {sizeConstraintSetId = a} :: SizeConstraintSetSummary)

-- | The name of the @SizeConstraintSet@, if any.
sizeConstraintSetSummary_name :: Lens.Lens' SizeConstraintSetSummary Prelude.Text
sizeConstraintSetSummary_name = Lens.lens (\SizeConstraintSetSummary' {name} -> name) (\s@SizeConstraintSetSummary' {} a -> s {name = a} :: SizeConstraintSetSummary)

instance Prelude.FromJSON SizeConstraintSetSummary where
  parseJSON =
    Prelude.withObject
      "SizeConstraintSetSummary"
      ( \x ->
          SizeConstraintSetSummary'
            Prelude.<$> (x Prelude..: "SizeConstraintSetId")
            Prelude.<*> (x Prelude..: "Name")
      )

instance Prelude.Hashable SizeConstraintSetSummary

instance Prelude.NFData SizeConstraintSetSummary
