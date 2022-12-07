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
-- Module      : Amazonka.SESV2.Types.AccountDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.AccountDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.ContactLanguage
import Amazonka.SESV2.Types.MailType
import Amazonka.SESV2.Types.ReviewDetails

-- | An object that contains information about your account details.
--
-- /See:/ 'newAccountDetails' smart constructor.
data AccountDetails = AccountDetails'
  { -- | The type of email your account is sending. The mail type can be one of
    -- the following:
    --
    -- -   @MARKETING@ – Most of your sending traffic is to keep your customers
    --     informed of your latest offering.
    --
    -- -   @TRANSACTIONAL@ – Most of your sending traffic is to communicate
    --     during a transaction with a customer.
    mailType :: Prelude.Maybe MailType,
    -- | The language you would prefer for the case. The contact language can be
    -- one of @ENGLISH@ or @JAPANESE@.
    contactLanguage :: Prelude.Maybe ContactLanguage,
    -- | A description of the types of email that you plan to send.
    useCaseDescription :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Information about the review of the latest details you submitted.
    reviewDetails :: Prelude.Maybe ReviewDetails,
    -- | The URL of your website. This information helps us better understand the
    -- type of content that you plan to send.
    websiteURL :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Additional email addresses where updates are sent about your account
    -- review process.
    additionalContactEmailAddresses :: Prelude.Maybe (Data.Sensitive (Prelude.NonEmpty (Data.Sensitive Prelude.Text)))
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mailType', 'accountDetails_mailType' - The type of email your account is sending. The mail type can be one of
-- the following:
--
-- -   @MARKETING@ – Most of your sending traffic is to keep your customers
--     informed of your latest offering.
--
-- -   @TRANSACTIONAL@ – Most of your sending traffic is to communicate
--     during a transaction with a customer.
--
-- 'contactLanguage', 'accountDetails_contactLanguage' - The language you would prefer for the case. The contact language can be
-- one of @ENGLISH@ or @JAPANESE@.
--
-- 'useCaseDescription', 'accountDetails_useCaseDescription' - A description of the types of email that you plan to send.
--
-- 'reviewDetails', 'accountDetails_reviewDetails' - Information about the review of the latest details you submitted.
--
-- 'websiteURL', 'accountDetails_websiteURL' - The URL of your website. This information helps us better understand the
-- type of content that you plan to send.
--
-- 'additionalContactEmailAddresses', 'accountDetails_additionalContactEmailAddresses' - Additional email addresses where updates are sent about your account
-- review process.
newAccountDetails ::
  AccountDetails
newAccountDetails =
  AccountDetails'
    { mailType = Prelude.Nothing,
      contactLanguage = Prelude.Nothing,
      useCaseDescription = Prelude.Nothing,
      reviewDetails = Prelude.Nothing,
      websiteURL = Prelude.Nothing,
      additionalContactEmailAddresses = Prelude.Nothing
    }

-- | The type of email your account is sending. The mail type can be one of
-- the following:
--
-- -   @MARKETING@ – Most of your sending traffic is to keep your customers
--     informed of your latest offering.
--
-- -   @TRANSACTIONAL@ – Most of your sending traffic is to communicate
--     during a transaction with a customer.
accountDetails_mailType :: Lens.Lens' AccountDetails (Prelude.Maybe MailType)
accountDetails_mailType = Lens.lens (\AccountDetails' {mailType} -> mailType) (\s@AccountDetails' {} a -> s {mailType = a} :: AccountDetails)

-- | The language you would prefer for the case. The contact language can be
-- one of @ENGLISH@ or @JAPANESE@.
accountDetails_contactLanguage :: Lens.Lens' AccountDetails (Prelude.Maybe ContactLanguage)
accountDetails_contactLanguage = Lens.lens (\AccountDetails' {contactLanguage} -> contactLanguage) (\s@AccountDetails' {} a -> s {contactLanguage = a} :: AccountDetails)

-- | A description of the types of email that you plan to send.
accountDetails_useCaseDescription :: Lens.Lens' AccountDetails (Prelude.Maybe Prelude.Text)
accountDetails_useCaseDescription = Lens.lens (\AccountDetails' {useCaseDescription} -> useCaseDescription) (\s@AccountDetails' {} a -> s {useCaseDescription = a} :: AccountDetails) Prelude.. Lens.mapping Data._Sensitive

-- | Information about the review of the latest details you submitted.
accountDetails_reviewDetails :: Lens.Lens' AccountDetails (Prelude.Maybe ReviewDetails)
accountDetails_reviewDetails = Lens.lens (\AccountDetails' {reviewDetails} -> reviewDetails) (\s@AccountDetails' {} a -> s {reviewDetails = a} :: AccountDetails)

-- | The URL of your website. This information helps us better understand the
-- type of content that you plan to send.
accountDetails_websiteURL :: Lens.Lens' AccountDetails (Prelude.Maybe Prelude.Text)
accountDetails_websiteURL = Lens.lens (\AccountDetails' {websiteURL} -> websiteURL) (\s@AccountDetails' {} a -> s {websiteURL = a} :: AccountDetails) Prelude.. Lens.mapping Data._Sensitive

-- | Additional email addresses where updates are sent about your account
-- review process.
accountDetails_additionalContactEmailAddresses :: Lens.Lens' AccountDetails (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
accountDetails_additionalContactEmailAddresses = Lens.lens (\AccountDetails' {additionalContactEmailAddresses} -> additionalContactEmailAddresses) (\s@AccountDetails' {} a -> s {additionalContactEmailAddresses = a} :: AccountDetails) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

instance Data.FromJSON AccountDetails where
  parseJSON =
    Data.withObject
      "AccountDetails"
      ( \x ->
          AccountDetails'
            Prelude.<$> (x Data..:? "MailType")
            Prelude.<*> (x Data..:? "ContactLanguage")
            Prelude.<*> (x Data..:? "UseCaseDescription")
            Prelude.<*> (x Data..:? "ReviewDetails")
            Prelude.<*> (x Data..:? "WebsiteURL")
            Prelude.<*> (x Data..:? "AdditionalContactEmailAddresses")
      )

instance Prelude.Hashable AccountDetails where
  hashWithSalt _salt AccountDetails' {..} =
    _salt `Prelude.hashWithSalt` mailType
      `Prelude.hashWithSalt` contactLanguage
      `Prelude.hashWithSalt` useCaseDescription
      `Prelude.hashWithSalt` reviewDetails
      `Prelude.hashWithSalt` websiteURL
      `Prelude.hashWithSalt` additionalContactEmailAddresses

instance Prelude.NFData AccountDetails where
  rnf AccountDetails' {..} =
    Prelude.rnf mailType
      `Prelude.seq` Prelude.rnf contactLanguage
      `Prelude.seq` Prelude.rnf useCaseDescription
      `Prelude.seq` Prelude.rnf reviewDetails
      `Prelude.seq` Prelude.rnf websiteURL
      `Prelude.seq` Prelude.rnf additionalContactEmailAddresses
