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
-- Module      : Network.AWS.WAF.Types.Predicate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.Predicate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAF.Types.PredicateType

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Specifies the ByteMatchSet, IPSet, SqlInjectionMatchSet, XssMatchSet,
-- RegexMatchSet, GeoMatchSet, and SizeConstraintSet objects that you want
-- to add to a @Rule@ and, for each object, indicates whether you want to
-- negate the settings, for example, requests that do NOT originate from
-- the IP address 192.0.2.44.
--
-- /See:/ 'newPredicate' smart constructor.
data Predicate = Predicate'
  { -- | Set @Negated@ to @False@ if you want AWS WAF to allow, block, or count
    -- requests based on the settings in the specified ByteMatchSet, IPSet,
    -- SqlInjectionMatchSet, XssMatchSet, RegexMatchSet, GeoMatchSet, or
    -- SizeConstraintSet. For example, if an @IPSet@ includes the IP address
    -- @192.0.2.44@, AWS WAF will allow or block requests based on that IP
    -- address.
    --
    -- Set @Negated@ to @True@ if you want AWS WAF to allow or block a request
    -- based on the negation of the settings in the ByteMatchSet, IPSet,
    -- SqlInjectionMatchSet, XssMatchSet, RegexMatchSet, GeoMatchSet, or
    -- SizeConstraintSet. For example, if an @IPSet@ includes the IP address
    -- @192.0.2.44@, AWS WAF will allow, block, or count requests based on all
    -- IP addresses /except/ @192.0.2.44@.
    negated :: Prelude.Bool,
    -- | The type of predicate in a @Rule@, such as @ByteMatch@ or @IPSet@.
    type' :: PredicateType,
    -- | A unique identifier for a predicate in a @Rule@, such as
    -- @ByteMatchSetId@ or @IPSetId@. The ID is returned by the corresponding
    -- @Create@ or @List@ command.
    dataId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Predicate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'negated', 'predicate_negated' - Set @Negated@ to @False@ if you want AWS WAF to allow, block, or count
-- requests based on the settings in the specified ByteMatchSet, IPSet,
-- SqlInjectionMatchSet, XssMatchSet, RegexMatchSet, GeoMatchSet, or
-- SizeConstraintSet. For example, if an @IPSet@ includes the IP address
-- @192.0.2.44@, AWS WAF will allow or block requests based on that IP
-- address.
--
-- Set @Negated@ to @True@ if you want AWS WAF to allow or block a request
-- based on the negation of the settings in the ByteMatchSet, IPSet,
-- SqlInjectionMatchSet, XssMatchSet, RegexMatchSet, GeoMatchSet, or
-- SizeConstraintSet. For example, if an @IPSet@ includes the IP address
-- @192.0.2.44@, AWS WAF will allow, block, or count requests based on all
-- IP addresses /except/ @192.0.2.44@.
--
-- 'type'', 'predicate_type' - The type of predicate in a @Rule@, such as @ByteMatch@ or @IPSet@.
--
-- 'dataId', 'predicate_dataId' - A unique identifier for a predicate in a @Rule@, such as
-- @ByteMatchSetId@ or @IPSetId@. The ID is returned by the corresponding
-- @Create@ or @List@ command.
newPredicate ::
  -- | 'negated'
  Prelude.Bool ->
  -- | 'type''
  PredicateType ->
  -- | 'dataId'
  Prelude.Text ->
  Predicate
newPredicate pNegated_ pType_ pDataId_ =
  Predicate'
    { negated = pNegated_,
      type' = pType_,
      dataId = pDataId_
    }

-- | Set @Negated@ to @False@ if you want AWS WAF to allow, block, or count
-- requests based on the settings in the specified ByteMatchSet, IPSet,
-- SqlInjectionMatchSet, XssMatchSet, RegexMatchSet, GeoMatchSet, or
-- SizeConstraintSet. For example, if an @IPSet@ includes the IP address
-- @192.0.2.44@, AWS WAF will allow or block requests based on that IP
-- address.
--
-- Set @Negated@ to @True@ if you want AWS WAF to allow or block a request
-- based on the negation of the settings in the ByteMatchSet, IPSet,
-- SqlInjectionMatchSet, XssMatchSet, RegexMatchSet, GeoMatchSet, or
-- SizeConstraintSet. For example, if an @IPSet@ includes the IP address
-- @192.0.2.44@, AWS WAF will allow, block, or count requests based on all
-- IP addresses /except/ @192.0.2.44@.
predicate_negated :: Lens.Lens' Predicate Prelude.Bool
predicate_negated = Lens.lens (\Predicate' {negated} -> negated) (\s@Predicate' {} a -> s {negated = a} :: Predicate)

-- | The type of predicate in a @Rule@, such as @ByteMatch@ or @IPSet@.
predicate_type :: Lens.Lens' Predicate PredicateType
predicate_type = Lens.lens (\Predicate' {type'} -> type') (\s@Predicate' {} a -> s {type' = a} :: Predicate)

-- | A unique identifier for a predicate in a @Rule@, such as
-- @ByteMatchSetId@ or @IPSetId@. The ID is returned by the corresponding
-- @Create@ or @List@ command.
predicate_dataId :: Lens.Lens' Predicate Prelude.Text
predicate_dataId = Lens.lens (\Predicate' {dataId} -> dataId) (\s@Predicate' {} a -> s {dataId = a} :: Predicate)

instance Prelude.FromJSON Predicate where
  parseJSON =
    Prelude.withObject
      "Predicate"
      ( \x ->
          Predicate'
            Prelude.<$> (x Prelude..: "Negated")
            Prelude.<*> (x Prelude..: "Type")
            Prelude.<*> (x Prelude..: "DataId")
      )

instance Prelude.Hashable Predicate

instance Prelude.NFData Predicate

instance Prelude.ToJSON Predicate where
  toJSON Predicate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Negated" Prelude..= negated),
            Prelude.Just ("Type" Prelude..= type'),
            Prelude.Just ("DataId" Prelude..= dataId)
          ]
      )
