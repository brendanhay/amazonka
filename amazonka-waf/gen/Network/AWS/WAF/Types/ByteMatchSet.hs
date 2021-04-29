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
-- Module      : Network.AWS.WAF.Types.ByteMatchSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.ByteMatchSet where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAF.Types.ByteMatchTuple

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- In a GetByteMatchSet request, @ByteMatchSet@ is a complex type that
-- contains the @ByteMatchSetId@ and @Name@ of a @ByteMatchSet@, and the
-- values that you specified when you updated the @ByteMatchSet@.
--
-- A complex type that contains @ByteMatchTuple@ objects, which specify the
-- parts of web requests that you want AWS WAF to inspect and the values
-- that you want AWS WAF to search for. If a @ByteMatchSet@ contains more
-- than one @ByteMatchTuple@ object, a request needs to match the settings
-- in only one @ByteMatchTuple@ to be considered a match.
--
-- /See:/ 'newByteMatchSet' smart constructor.
data ByteMatchSet = ByteMatchSet'
  { -- | A friendly name or description of the ByteMatchSet. You can\'t change
    -- @Name@ after you create a @ByteMatchSet@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The @ByteMatchSetId@ for a @ByteMatchSet@. You use @ByteMatchSetId@ to
    -- get information about a @ByteMatchSet@ (see GetByteMatchSet), update a
    -- @ByteMatchSet@ (see UpdateByteMatchSet), insert a @ByteMatchSet@ into a
    -- @Rule@ or delete one from a @Rule@ (see UpdateRule), and delete a
    -- @ByteMatchSet@ from AWS WAF (see DeleteByteMatchSet).
    --
    -- @ByteMatchSetId@ is returned by CreateByteMatchSet and by
    -- ListByteMatchSets.
    byteMatchSetId :: Prelude.Text,
    -- | Specifies the bytes (typically a string that corresponds with ASCII
    -- characters) that you want AWS WAF to search for in web requests, the
    -- location in requests that you want AWS WAF to search, and other
    -- settings.
    byteMatchTuples :: [ByteMatchTuple]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ByteMatchSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'byteMatchSet_name' - A friendly name or description of the ByteMatchSet. You can\'t change
-- @Name@ after you create a @ByteMatchSet@.
--
-- 'byteMatchSetId', 'byteMatchSet_byteMatchSetId' - The @ByteMatchSetId@ for a @ByteMatchSet@. You use @ByteMatchSetId@ to
-- get information about a @ByteMatchSet@ (see GetByteMatchSet), update a
-- @ByteMatchSet@ (see UpdateByteMatchSet), insert a @ByteMatchSet@ into a
-- @Rule@ or delete one from a @Rule@ (see UpdateRule), and delete a
-- @ByteMatchSet@ from AWS WAF (see DeleteByteMatchSet).
--
-- @ByteMatchSetId@ is returned by CreateByteMatchSet and by
-- ListByteMatchSets.
--
-- 'byteMatchTuples', 'byteMatchSet_byteMatchTuples' - Specifies the bytes (typically a string that corresponds with ASCII
-- characters) that you want AWS WAF to search for in web requests, the
-- location in requests that you want AWS WAF to search, and other
-- settings.
newByteMatchSet ::
  -- | 'byteMatchSetId'
  Prelude.Text ->
  ByteMatchSet
newByteMatchSet pByteMatchSetId_ =
  ByteMatchSet'
    { name = Prelude.Nothing,
      byteMatchSetId = pByteMatchSetId_,
      byteMatchTuples = Prelude.mempty
    }

-- | A friendly name or description of the ByteMatchSet. You can\'t change
-- @Name@ after you create a @ByteMatchSet@.
byteMatchSet_name :: Lens.Lens' ByteMatchSet (Prelude.Maybe Prelude.Text)
byteMatchSet_name = Lens.lens (\ByteMatchSet' {name} -> name) (\s@ByteMatchSet' {} a -> s {name = a} :: ByteMatchSet)

-- | The @ByteMatchSetId@ for a @ByteMatchSet@. You use @ByteMatchSetId@ to
-- get information about a @ByteMatchSet@ (see GetByteMatchSet), update a
-- @ByteMatchSet@ (see UpdateByteMatchSet), insert a @ByteMatchSet@ into a
-- @Rule@ or delete one from a @Rule@ (see UpdateRule), and delete a
-- @ByteMatchSet@ from AWS WAF (see DeleteByteMatchSet).
--
-- @ByteMatchSetId@ is returned by CreateByteMatchSet and by
-- ListByteMatchSets.
byteMatchSet_byteMatchSetId :: Lens.Lens' ByteMatchSet Prelude.Text
byteMatchSet_byteMatchSetId = Lens.lens (\ByteMatchSet' {byteMatchSetId} -> byteMatchSetId) (\s@ByteMatchSet' {} a -> s {byteMatchSetId = a} :: ByteMatchSet)

-- | Specifies the bytes (typically a string that corresponds with ASCII
-- characters) that you want AWS WAF to search for in web requests, the
-- location in requests that you want AWS WAF to search, and other
-- settings.
byteMatchSet_byteMatchTuples :: Lens.Lens' ByteMatchSet [ByteMatchTuple]
byteMatchSet_byteMatchTuples = Lens.lens (\ByteMatchSet' {byteMatchTuples} -> byteMatchTuples) (\s@ByteMatchSet' {} a -> s {byteMatchTuples = a} :: ByteMatchSet) Prelude.. Prelude._Coerce

instance Prelude.FromJSON ByteMatchSet where
  parseJSON =
    Prelude.withObject
      "ByteMatchSet"
      ( \x ->
          ByteMatchSet'
            Prelude.<$> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..: "ByteMatchSetId")
            Prelude.<*> ( x Prelude..:? "ByteMatchTuples"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ByteMatchSet

instance Prelude.NFData ByteMatchSet
