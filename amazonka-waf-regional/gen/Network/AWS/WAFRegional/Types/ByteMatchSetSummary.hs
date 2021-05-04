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
-- Module      : Network.AWS.WAFRegional.Types.ByteMatchSetSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.ByteMatchSetSummary where

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
-- Returned by ListByteMatchSets. Each @ByteMatchSetSummary@ object
-- includes the @Name@ and @ByteMatchSetId@ for one ByteMatchSet.
--
-- /See:/ 'newByteMatchSetSummary' smart constructor.
data ByteMatchSetSummary = ByteMatchSetSummary'
  { -- | The @ByteMatchSetId@ for a @ByteMatchSet@. You use @ByteMatchSetId@ to
    -- get information about a @ByteMatchSet@, update a @ByteMatchSet@, remove
    -- a @ByteMatchSet@ from a @Rule@, and delete a @ByteMatchSet@ from AWS
    -- WAF.
    --
    -- @ByteMatchSetId@ is returned by CreateByteMatchSet and by
    -- ListByteMatchSets.
    byteMatchSetId :: Prelude.Text,
    -- | A friendly name or description of the ByteMatchSet. You can\'t change
    -- @Name@ after you create a @ByteMatchSet@.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ByteMatchSetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'byteMatchSetId', 'byteMatchSetSummary_byteMatchSetId' - The @ByteMatchSetId@ for a @ByteMatchSet@. You use @ByteMatchSetId@ to
-- get information about a @ByteMatchSet@, update a @ByteMatchSet@, remove
-- a @ByteMatchSet@ from a @Rule@, and delete a @ByteMatchSet@ from AWS
-- WAF.
--
-- @ByteMatchSetId@ is returned by CreateByteMatchSet and by
-- ListByteMatchSets.
--
-- 'name', 'byteMatchSetSummary_name' - A friendly name or description of the ByteMatchSet. You can\'t change
-- @Name@ after you create a @ByteMatchSet@.
newByteMatchSetSummary ::
  -- | 'byteMatchSetId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  ByteMatchSetSummary
newByteMatchSetSummary pByteMatchSetId_ pName_ =
  ByteMatchSetSummary'
    { byteMatchSetId =
        pByteMatchSetId_,
      name = pName_
    }

-- | The @ByteMatchSetId@ for a @ByteMatchSet@. You use @ByteMatchSetId@ to
-- get information about a @ByteMatchSet@, update a @ByteMatchSet@, remove
-- a @ByteMatchSet@ from a @Rule@, and delete a @ByteMatchSet@ from AWS
-- WAF.
--
-- @ByteMatchSetId@ is returned by CreateByteMatchSet and by
-- ListByteMatchSets.
byteMatchSetSummary_byteMatchSetId :: Lens.Lens' ByteMatchSetSummary Prelude.Text
byteMatchSetSummary_byteMatchSetId = Lens.lens (\ByteMatchSetSummary' {byteMatchSetId} -> byteMatchSetId) (\s@ByteMatchSetSummary' {} a -> s {byteMatchSetId = a} :: ByteMatchSetSummary)

-- | A friendly name or description of the ByteMatchSet. You can\'t change
-- @Name@ after you create a @ByteMatchSet@.
byteMatchSetSummary_name :: Lens.Lens' ByteMatchSetSummary Prelude.Text
byteMatchSetSummary_name = Lens.lens (\ByteMatchSetSummary' {name} -> name) (\s@ByteMatchSetSummary' {} a -> s {name = a} :: ByteMatchSetSummary)

instance Prelude.FromJSON ByteMatchSetSummary where
  parseJSON =
    Prelude.withObject
      "ByteMatchSetSummary"
      ( \x ->
          ByteMatchSetSummary'
            Prelude.<$> (x Prelude..: "ByteMatchSetId")
            Prelude.<*> (x Prelude..: "Name")
      )

instance Prelude.Hashable ByteMatchSetSummary

instance Prelude.NFData ByteMatchSetSummary
