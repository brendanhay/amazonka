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
-- Module      : Network.AWS.SESv2.Types.AccountDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.AccountDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SESv2.Types.ContactLanguage
import Network.AWS.SESv2.Types.MailType
import Network.AWS.SESv2.Types.ReviewDetails

-- | An object that contains information about your account details.
--
-- /See:/ 'newAccountDetails' smart constructor.
data AccountDetails = AccountDetails'
  { -- | A description of the types of email that you plan to send.
    useCaseDescription :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The type of email your account is sending. The mail type can be one of
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
    -- | The URL of your website. This information helps us better understand the
    -- type of content that you plan to send.
    websiteURL :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Information about the review of the latest details you submitted.
    reviewDetails :: Prelude.Maybe ReviewDetails,
    -- | Additional email addresses where updates are sent about your account
    -- review process.
    additionalContactEmailAddresses :: Prelude.Maybe (Core.Sensitive (Prelude.NonEmpty (Core.Sensitive Prelude.Text)))
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
-- 'useCaseDescription', 'accountDetails_useCaseDescription' - A description of the types of email that you plan to send.
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
-- 'websiteURL', 'accountDetails_websiteURL' - The URL of your website. This information helps us better understand the
-- type of content that you plan to send.
--
-- 'reviewDetails', 'accountDetails_reviewDetails' - Information about the review of the latest details you submitted.
--
-- 'additionalContactEmailAddresses', 'accountDetails_additionalContactEmailAddresses' - Additional email addresses where updates are sent about your account
-- review process.
newAccountDetails ::
  AccountDetails
newAccountDetails =
  AccountDetails'
    { useCaseDescription =
        Prelude.Nothing,
      mailType = Prelude.Nothing,
      contactLanguage = Prelude.Nothing,
      websiteURL = Prelude.Nothing,
      reviewDetails = Prelude.Nothing,
      additionalContactEmailAddresses = Prelude.Nothing
    }

-- | A description of the types of email that you plan to send.
accountDetails_useCaseDescription :: Lens.Lens' AccountDetails (Prelude.Maybe Prelude.Text)
accountDetails_useCaseDescription = Lens.lens (\AccountDetails' {useCaseDescription} -> useCaseDescription) (\s@AccountDetails' {} a -> s {useCaseDescription = a} :: AccountDetails) Prelude.. Lens.mapping Core._Sensitive

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

-- | The URL of your website. This information helps us better understand the
-- type of content that you plan to send.
accountDetails_websiteURL :: Lens.Lens' AccountDetails (Prelude.Maybe Prelude.Text)
accountDetails_websiteURL = Lens.lens (\AccountDetails' {websiteURL} -> websiteURL) (\s@AccountDetails' {} a -> s {websiteURL = a} :: AccountDetails) Prelude.. Lens.mapping Core._Sensitive

-- | Information about the review of the latest details you submitted.
accountDetails_reviewDetails :: Lens.Lens' AccountDetails (Prelude.Maybe ReviewDetails)
accountDetails_reviewDetails = Lens.lens (\AccountDetails' {reviewDetails} -> reviewDetails) (\s@AccountDetails' {} a -> s {reviewDetails = a} :: AccountDetails)

-- | Additional email addresses where updates are sent about your account
-- review process.
accountDetails_additionalContactEmailAddresses :: Lens.Lens' AccountDetails (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
accountDetails_additionalContactEmailAddresses = Lens.lens (\AccountDetails' {additionalContactEmailAddresses} -> additionalContactEmailAddresses) (\s@AccountDetails' {} a -> s {additionalContactEmailAddresses = a} :: AccountDetails) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens._Coerce)

instance Core.FromJSON AccountDetails where
  parseJSON =
    Core.withObject
      "AccountDetails"
      ( \x ->
          AccountDetails'
            Prelude.<$> (x Core..:? "UseCaseDescription")
            Prelude.<*> (x Core..:? "MailType")
            Prelude.<*> (x Core..:? "ContactLanguage")
            Prelude.<*> (x Core..:? "WebsiteURL")
            Prelude.<*> (x Core..:? "ReviewDetails")
            Prelude.<*> (x Core..:? "AdditionalContactEmailAddresses")
      )

instance Prelude.Hashable AccountDetails

instance Prelude.NFData AccountDetails
