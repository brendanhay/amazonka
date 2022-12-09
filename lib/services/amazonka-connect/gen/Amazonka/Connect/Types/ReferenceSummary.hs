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
-- Module      : Amazonka.Connect.Types.ReferenceSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.ReferenceSummary where

import Amazonka.Connect.Types.AttachmentReference
import Amazonka.Connect.Types.DateReference
import Amazonka.Connect.Types.EmailReference
import Amazonka.Connect.Types.NumberReference
import Amazonka.Connect.Types.StringReference
import Amazonka.Connect.Types.UrlReference
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains summary information about a reference. @ReferenceSummary@
-- contains only one non null field between the URL and attachment based on
-- the reference type.
--
-- /See:/ 'newReferenceSummary' smart constructor.
data ReferenceSummary = ReferenceSummary'
  { -- | Information about the reference when the @referenceType@ is
    -- @ATTACHMENT@. Otherwise, null.
    attachment :: Prelude.Maybe AttachmentReference,
    -- | Information about a reference when the @referenceType@ is @DATE@.
    -- Otherwise, null.
    date :: Prelude.Maybe DateReference,
    -- | Information about a reference when the @referenceType@ is @EMAIL@.
    -- Otherwise, null.
    email :: Prelude.Maybe EmailReference,
    -- | Information about a reference when the @referenceType@ is @NUMBER@.
    -- Otherwise, null.
    number :: Prelude.Maybe NumberReference,
    -- | Information about a reference when the @referenceType@ is @STRING@.
    -- Otherwise, null.
    string :: Prelude.Maybe StringReference,
    -- | Information about the reference when the @referenceType@ is @URL@.
    -- Otherwise, null.
    url :: Prelude.Maybe UrlReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferenceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachment', 'referenceSummary_attachment' - Information about the reference when the @referenceType@ is
-- @ATTACHMENT@. Otherwise, null.
--
-- 'date', 'referenceSummary_date' - Information about a reference when the @referenceType@ is @DATE@.
-- Otherwise, null.
--
-- 'email', 'referenceSummary_email' - Information about a reference when the @referenceType@ is @EMAIL@.
-- Otherwise, null.
--
-- 'number', 'referenceSummary_number' - Information about a reference when the @referenceType@ is @NUMBER@.
-- Otherwise, null.
--
-- 'string', 'referenceSummary_string' - Information about a reference when the @referenceType@ is @STRING@.
-- Otherwise, null.
--
-- 'url', 'referenceSummary_url' - Information about the reference when the @referenceType@ is @URL@.
-- Otherwise, null.
newReferenceSummary ::
  ReferenceSummary
newReferenceSummary =
  ReferenceSummary'
    { attachment = Prelude.Nothing,
      date = Prelude.Nothing,
      email = Prelude.Nothing,
      number = Prelude.Nothing,
      string = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | Information about the reference when the @referenceType@ is
-- @ATTACHMENT@. Otherwise, null.
referenceSummary_attachment :: Lens.Lens' ReferenceSummary (Prelude.Maybe AttachmentReference)
referenceSummary_attachment = Lens.lens (\ReferenceSummary' {attachment} -> attachment) (\s@ReferenceSummary' {} a -> s {attachment = a} :: ReferenceSummary)

-- | Information about a reference when the @referenceType@ is @DATE@.
-- Otherwise, null.
referenceSummary_date :: Lens.Lens' ReferenceSummary (Prelude.Maybe DateReference)
referenceSummary_date = Lens.lens (\ReferenceSummary' {date} -> date) (\s@ReferenceSummary' {} a -> s {date = a} :: ReferenceSummary)

-- | Information about a reference when the @referenceType@ is @EMAIL@.
-- Otherwise, null.
referenceSummary_email :: Lens.Lens' ReferenceSummary (Prelude.Maybe EmailReference)
referenceSummary_email = Lens.lens (\ReferenceSummary' {email} -> email) (\s@ReferenceSummary' {} a -> s {email = a} :: ReferenceSummary)

-- | Information about a reference when the @referenceType@ is @NUMBER@.
-- Otherwise, null.
referenceSummary_number :: Lens.Lens' ReferenceSummary (Prelude.Maybe NumberReference)
referenceSummary_number = Lens.lens (\ReferenceSummary' {number} -> number) (\s@ReferenceSummary' {} a -> s {number = a} :: ReferenceSummary)

-- | Information about a reference when the @referenceType@ is @STRING@.
-- Otherwise, null.
referenceSummary_string :: Lens.Lens' ReferenceSummary (Prelude.Maybe StringReference)
referenceSummary_string = Lens.lens (\ReferenceSummary' {string} -> string) (\s@ReferenceSummary' {} a -> s {string = a} :: ReferenceSummary)

-- | Information about the reference when the @referenceType@ is @URL@.
-- Otherwise, null.
referenceSummary_url :: Lens.Lens' ReferenceSummary (Prelude.Maybe UrlReference)
referenceSummary_url = Lens.lens (\ReferenceSummary' {url} -> url) (\s@ReferenceSummary' {} a -> s {url = a} :: ReferenceSummary)

instance Data.FromJSON ReferenceSummary where
  parseJSON =
    Data.withObject
      "ReferenceSummary"
      ( \x ->
          ReferenceSummary'
            Prelude.<$> (x Data..:? "Attachment")
            Prelude.<*> (x Data..:? "Date")
            Prelude.<*> (x Data..:? "Email")
            Prelude.<*> (x Data..:? "Number")
            Prelude.<*> (x Data..:? "String")
            Prelude.<*> (x Data..:? "Url")
      )

instance Prelude.Hashable ReferenceSummary where
  hashWithSalt _salt ReferenceSummary' {..} =
    _salt `Prelude.hashWithSalt` attachment
      `Prelude.hashWithSalt` date
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` number
      `Prelude.hashWithSalt` string
      `Prelude.hashWithSalt` url

instance Prelude.NFData ReferenceSummary where
  rnf ReferenceSummary' {..} =
    Prelude.rnf attachment
      `Prelude.seq` Prelude.rnf date
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf number
      `Prelude.seq` Prelude.rnf string
      `Prelude.seq` Prelude.rnf url
