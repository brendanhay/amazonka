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
-- Module      : Amazonka.WAFV2.Types.AWSManagedRulesACFPRuleSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.AWSManagedRulesACFPRuleSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.RequestInspectionACFP
import Amazonka.WAFV2.Types.ResponseInspection

-- | Details for your use of the account creation fraud prevention managed
-- rule group, @AWSManagedRulesACFPRuleSet@. This configuration is used in
-- @ManagedRuleGroupConfig@.
--
-- /See:/ 'newAWSManagedRulesACFPRuleSet' smart constructor.
data AWSManagedRulesACFPRuleSet = AWSManagedRulesACFPRuleSet'
  { -- | Allow the use of regular expressions in the registration page path and
    -- the account creation path.
    enableRegexInPath :: Prelude.Maybe Prelude.Bool,
    -- | The criteria for inspecting responses to account creation requests, used
    -- by the ACFP rule group to track account creation success rates.
    --
    -- Response inspection is available only in web ACLs that protect Amazon
    -- CloudFront distributions.
    --
    -- The ACFP rule group evaluates the responses that your protected
    -- resources send back to client account creation attempts, keeping count
    -- of successful and failed attempts from each IP address and client
    -- session. Using this information, the rule group labels and mitigates
    -- requests from client sessions and IP addresses that have had too many
    -- successful account creation attempts in a short amount of time.
    responseInspection :: Prelude.Maybe ResponseInspection,
    -- | The path of the account creation endpoint for your application. This is
    -- the page on your website that accepts the completed registration form
    -- for a new user. This page must accept @POST@ requests.
    --
    -- For example, for the URL @https:\/\/example.com\/web\/signup@, you would
    -- provide the path @\/web\/signup@.
    creationPath :: Prelude.Text,
    -- | The path of the account registration endpoint for your application. This
    -- is the page on your website that presents the registration form to new
    -- users.
    --
    -- This page must accept @GET@ text\/html requests.
    --
    -- For example, for the URL @https:\/\/example.com\/web\/register@, you
    -- would provide the path @\/web\/register@.
    registrationPagePath :: Prelude.Text,
    -- | The criteria for inspecting account creation requests, used by the ACFP
    -- rule group to validate and track account creation attempts.
    requestInspection :: RequestInspectionACFP
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AWSManagedRulesACFPRuleSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableRegexInPath', 'aWSManagedRulesACFPRuleSet_enableRegexInPath' - Allow the use of regular expressions in the registration page path and
-- the account creation path.
--
-- 'responseInspection', 'aWSManagedRulesACFPRuleSet_responseInspection' - The criteria for inspecting responses to account creation requests, used
-- by the ACFP rule group to track account creation success rates.
--
-- Response inspection is available only in web ACLs that protect Amazon
-- CloudFront distributions.
--
-- The ACFP rule group evaluates the responses that your protected
-- resources send back to client account creation attempts, keeping count
-- of successful and failed attempts from each IP address and client
-- session. Using this information, the rule group labels and mitigates
-- requests from client sessions and IP addresses that have had too many
-- successful account creation attempts in a short amount of time.
--
-- 'creationPath', 'aWSManagedRulesACFPRuleSet_creationPath' - The path of the account creation endpoint for your application. This is
-- the page on your website that accepts the completed registration form
-- for a new user. This page must accept @POST@ requests.
--
-- For example, for the URL @https:\/\/example.com\/web\/signup@, you would
-- provide the path @\/web\/signup@.
--
-- 'registrationPagePath', 'aWSManagedRulesACFPRuleSet_registrationPagePath' - The path of the account registration endpoint for your application. This
-- is the page on your website that presents the registration form to new
-- users.
--
-- This page must accept @GET@ text\/html requests.
--
-- For example, for the URL @https:\/\/example.com\/web\/register@, you
-- would provide the path @\/web\/register@.
--
-- 'requestInspection', 'aWSManagedRulesACFPRuleSet_requestInspection' - The criteria for inspecting account creation requests, used by the ACFP
-- rule group to validate and track account creation attempts.
newAWSManagedRulesACFPRuleSet ::
  -- | 'creationPath'
  Prelude.Text ->
  -- | 'registrationPagePath'
  Prelude.Text ->
  -- | 'requestInspection'
  RequestInspectionACFP ->
  AWSManagedRulesACFPRuleSet
newAWSManagedRulesACFPRuleSet
  pCreationPath_
  pRegistrationPagePath_
  pRequestInspection_ =
    AWSManagedRulesACFPRuleSet'
      { enableRegexInPath =
          Prelude.Nothing,
        responseInspection = Prelude.Nothing,
        creationPath = pCreationPath_,
        registrationPagePath = pRegistrationPagePath_,
        requestInspection = pRequestInspection_
      }

-- | Allow the use of regular expressions in the registration page path and
-- the account creation path.
aWSManagedRulesACFPRuleSet_enableRegexInPath :: Lens.Lens' AWSManagedRulesACFPRuleSet (Prelude.Maybe Prelude.Bool)
aWSManagedRulesACFPRuleSet_enableRegexInPath = Lens.lens (\AWSManagedRulesACFPRuleSet' {enableRegexInPath} -> enableRegexInPath) (\s@AWSManagedRulesACFPRuleSet' {} a -> s {enableRegexInPath = a} :: AWSManagedRulesACFPRuleSet)

-- | The criteria for inspecting responses to account creation requests, used
-- by the ACFP rule group to track account creation success rates.
--
-- Response inspection is available only in web ACLs that protect Amazon
-- CloudFront distributions.
--
-- The ACFP rule group evaluates the responses that your protected
-- resources send back to client account creation attempts, keeping count
-- of successful and failed attempts from each IP address and client
-- session. Using this information, the rule group labels and mitigates
-- requests from client sessions and IP addresses that have had too many
-- successful account creation attempts in a short amount of time.
aWSManagedRulesACFPRuleSet_responseInspection :: Lens.Lens' AWSManagedRulesACFPRuleSet (Prelude.Maybe ResponseInspection)
aWSManagedRulesACFPRuleSet_responseInspection = Lens.lens (\AWSManagedRulesACFPRuleSet' {responseInspection} -> responseInspection) (\s@AWSManagedRulesACFPRuleSet' {} a -> s {responseInspection = a} :: AWSManagedRulesACFPRuleSet)

-- | The path of the account creation endpoint for your application. This is
-- the page on your website that accepts the completed registration form
-- for a new user. This page must accept @POST@ requests.
--
-- For example, for the URL @https:\/\/example.com\/web\/signup@, you would
-- provide the path @\/web\/signup@.
aWSManagedRulesACFPRuleSet_creationPath :: Lens.Lens' AWSManagedRulesACFPRuleSet Prelude.Text
aWSManagedRulesACFPRuleSet_creationPath = Lens.lens (\AWSManagedRulesACFPRuleSet' {creationPath} -> creationPath) (\s@AWSManagedRulesACFPRuleSet' {} a -> s {creationPath = a} :: AWSManagedRulesACFPRuleSet)

-- | The path of the account registration endpoint for your application. This
-- is the page on your website that presents the registration form to new
-- users.
--
-- This page must accept @GET@ text\/html requests.
--
-- For example, for the URL @https:\/\/example.com\/web\/register@, you
-- would provide the path @\/web\/register@.
aWSManagedRulesACFPRuleSet_registrationPagePath :: Lens.Lens' AWSManagedRulesACFPRuleSet Prelude.Text
aWSManagedRulesACFPRuleSet_registrationPagePath = Lens.lens (\AWSManagedRulesACFPRuleSet' {registrationPagePath} -> registrationPagePath) (\s@AWSManagedRulesACFPRuleSet' {} a -> s {registrationPagePath = a} :: AWSManagedRulesACFPRuleSet)

-- | The criteria for inspecting account creation requests, used by the ACFP
-- rule group to validate and track account creation attempts.
aWSManagedRulesACFPRuleSet_requestInspection :: Lens.Lens' AWSManagedRulesACFPRuleSet RequestInspectionACFP
aWSManagedRulesACFPRuleSet_requestInspection = Lens.lens (\AWSManagedRulesACFPRuleSet' {requestInspection} -> requestInspection) (\s@AWSManagedRulesACFPRuleSet' {} a -> s {requestInspection = a} :: AWSManagedRulesACFPRuleSet)

instance Data.FromJSON AWSManagedRulesACFPRuleSet where
  parseJSON =
    Data.withObject
      "AWSManagedRulesACFPRuleSet"
      ( \x ->
          AWSManagedRulesACFPRuleSet'
            Prelude.<$> (x Data..:? "EnableRegexInPath")
            Prelude.<*> (x Data..:? "ResponseInspection")
            Prelude.<*> (x Data..: "CreationPath")
            Prelude.<*> (x Data..: "RegistrationPagePath")
            Prelude.<*> (x Data..: "RequestInspection")
      )

instance Prelude.Hashable AWSManagedRulesACFPRuleSet where
  hashWithSalt _salt AWSManagedRulesACFPRuleSet' {..} =
    _salt
      `Prelude.hashWithSalt` enableRegexInPath
      `Prelude.hashWithSalt` responseInspection
      `Prelude.hashWithSalt` creationPath
      `Prelude.hashWithSalt` registrationPagePath
      `Prelude.hashWithSalt` requestInspection

instance Prelude.NFData AWSManagedRulesACFPRuleSet where
  rnf AWSManagedRulesACFPRuleSet' {..} =
    Prelude.rnf enableRegexInPath
      `Prelude.seq` Prelude.rnf responseInspection
      `Prelude.seq` Prelude.rnf creationPath
      `Prelude.seq` Prelude.rnf registrationPagePath
      `Prelude.seq` Prelude.rnf requestInspection

instance Data.ToJSON AWSManagedRulesACFPRuleSet where
  toJSON AWSManagedRulesACFPRuleSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EnableRegexInPath" Data..=)
              Prelude.<$> enableRegexInPath,
            ("ResponseInspection" Data..=)
              Prelude.<$> responseInspection,
            Prelude.Just ("CreationPath" Data..= creationPath),
            Prelude.Just
              ( "RegistrationPagePath"
                  Data..= registrationPagePath
              ),
            Prelude.Just
              ("RequestInspection" Data..= requestInspection)
          ]
      )
