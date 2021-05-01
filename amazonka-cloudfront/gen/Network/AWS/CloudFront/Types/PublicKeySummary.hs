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
-- Module      : Network.AWS.CloudFront.Types.PublicKeySummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.PublicKeySummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a public key.
--
-- /See:/ 'newPublicKeySummary' smart constructor.
data PublicKeySummary = PublicKeySummary'
  { -- | A comment to describe the public key.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the public key.
    id :: Prelude.Text,
    -- | A name to help identify the public key.
    name :: Prelude.Text,
    -- | The date and time when the public key was uploaded.
    createdTime :: Prelude.ISO8601,
    -- | The public key.
    encodedKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PublicKeySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'publicKeySummary_comment' - A comment to describe the public key.
--
-- 'id', 'publicKeySummary_id' - The identifier of the public key.
--
-- 'name', 'publicKeySummary_name' - A name to help identify the public key.
--
-- 'createdTime', 'publicKeySummary_createdTime' - The date and time when the public key was uploaded.
--
-- 'encodedKey', 'publicKeySummary_encodedKey' - The public key.
newPublicKeySummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  -- | 'encodedKey'
  Prelude.Text ->
  PublicKeySummary
newPublicKeySummary
  pId_
  pName_
  pCreatedTime_
  pEncodedKey_ =
    PublicKeySummary'
      { comment = Prelude.Nothing,
        id = pId_,
        name = pName_,
        createdTime = Prelude._Time Lens.# pCreatedTime_,
        encodedKey = pEncodedKey_
      }

-- | A comment to describe the public key.
publicKeySummary_comment :: Lens.Lens' PublicKeySummary (Prelude.Maybe Prelude.Text)
publicKeySummary_comment = Lens.lens (\PublicKeySummary' {comment} -> comment) (\s@PublicKeySummary' {} a -> s {comment = a} :: PublicKeySummary)

-- | The identifier of the public key.
publicKeySummary_id :: Lens.Lens' PublicKeySummary Prelude.Text
publicKeySummary_id = Lens.lens (\PublicKeySummary' {id} -> id) (\s@PublicKeySummary' {} a -> s {id = a} :: PublicKeySummary)

-- | A name to help identify the public key.
publicKeySummary_name :: Lens.Lens' PublicKeySummary Prelude.Text
publicKeySummary_name = Lens.lens (\PublicKeySummary' {name} -> name) (\s@PublicKeySummary' {} a -> s {name = a} :: PublicKeySummary)

-- | The date and time when the public key was uploaded.
publicKeySummary_createdTime :: Lens.Lens' PublicKeySummary Prelude.UTCTime
publicKeySummary_createdTime = Lens.lens (\PublicKeySummary' {createdTime} -> createdTime) (\s@PublicKeySummary' {} a -> s {createdTime = a} :: PublicKeySummary) Prelude.. Prelude._Time

-- | The public key.
publicKeySummary_encodedKey :: Lens.Lens' PublicKeySummary Prelude.Text
publicKeySummary_encodedKey = Lens.lens (\PublicKeySummary' {encodedKey} -> encodedKey) (\s@PublicKeySummary' {} a -> s {encodedKey = a} :: PublicKeySummary)

instance Prelude.FromXML PublicKeySummary where
  parseXML x =
    PublicKeySummary'
      Prelude.<$> (x Prelude..@? "Comment")
      Prelude.<*> (x Prelude..@ "Id")
      Prelude.<*> (x Prelude..@ "Name")
      Prelude.<*> (x Prelude..@ "CreatedTime")
      Prelude.<*> (x Prelude..@ "EncodedKey")

instance Prelude.Hashable PublicKeySummary

instance Prelude.NFData PublicKeySummary
