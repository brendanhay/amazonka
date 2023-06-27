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
-- Module      : Amazonka.WAFV2.Types.AWSManagedRulesATPRuleSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.AWSManagedRulesATPRuleSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.RequestInspection
import Amazonka.WAFV2.Types.ResponseInspection

-- | Details for your use of the account takeover prevention managed rule
-- group, @AWSManagedRulesATPRuleSet@. This configuration is used in
-- @ManagedRuleGroupConfig@.
--
-- /See:/ 'newAWSManagedRulesATPRuleSet' smart constructor.
data AWSManagedRulesATPRuleSet = AWSManagedRulesATPRuleSet'
  { -- | Allow the use of regular expressions in the login page path.
    enableRegexInPath :: Prelude.Maybe Prelude.Bool,
    -- | The criteria for inspecting login requests, used by the ATP rule group
    -- to validate credentials usage.
    requestInspection :: Prelude.Maybe RequestInspection,
    -- | The criteria for inspecting responses to login requests, used by the ATP
    -- rule group to track login failure rates.
    --
    -- Response inspection is available only in web ACLs that protect Amazon
    -- CloudFront distributions.
    --
    -- The ATP rule group evaluates the responses that your protected resources
    -- send back to client login attempts, keeping count of successful and
    -- failed attempts for each IP address and client session. Using this
    -- information, the rule group labels and mitigates requests from client
    -- sessions and IP addresses that have had too many failed login attempts
    -- in a short amount of time.
    responseInspection :: Prelude.Maybe ResponseInspection,
    -- | The path of the login endpoint for your application. For example, for
    -- the URL @https:\/\/example.com\/web\/login@, you would provide the path
    -- @\/web\/login@.
    --
    -- The rule group inspects only HTTP @POST@ requests to your specified
    -- login endpoint.
    loginPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AWSManagedRulesATPRuleSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableRegexInPath', 'aWSManagedRulesATPRuleSet_enableRegexInPath' - Allow the use of regular expressions in the login page path.
--
-- 'requestInspection', 'aWSManagedRulesATPRuleSet_requestInspection' - The criteria for inspecting login requests, used by the ATP rule group
-- to validate credentials usage.
--
-- 'responseInspection', 'aWSManagedRulesATPRuleSet_responseInspection' - The criteria for inspecting responses to login requests, used by the ATP
-- rule group to track login failure rates.
--
-- Response inspection is available only in web ACLs that protect Amazon
-- CloudFront distributions.
--
-- The ATP rule group evaluates the responses that your protected resources
-- send back to client login attempts, keeping count of successful and
-- failed attempts for each IP address and client session. Using this
-- information, the rule group labels and mitigates requests from client
-- sessions and IP addresses that have had too many failed login attempts
-- in a short amount of time.
--
-- 'loginPath', 'aWSManagedRulesATPRuleSet_loginPath' - The path of the login endpoint for your application. For example, for
-- the URL @https:\/\/example.com\/web\/login@, you would provide the path
-- @\/web\/login@.
--
-- The rule group inspects only HTTP @POST@ requests to your specified
-- login endpoint.
newAWSManagedRulesATPRuleSet ::
  -- | 'loginPath'
  Prelude.Text ->
  AWSManagedRulesATPRuleSet
newAWSManagedRulesATPRuleSet pLoginPath_ =
  AWSManagedRulesATPRuleSet'
    { enableRegexInPath =
        Prelude.Nothing,
      requestInspection = Prelude.Nothing,
      responseInspection = Prelude.Nothing,
      loginPath = pLoginPath_
    }

-- | Allow the use of regular expressions in the login page path.
aWSManagedRulesATPRuleSet_enableRegexInPath :: Lens.Lens' AWSManagedRulesATPRuleSet (Prelude.Maybe Prelude.Bool)
aWSManagedRulesATPRuleSet_enableRegexInPath = Lens.lens (\AWSManagedRulesATPRuleSet' {enableRegexInPath} -> enableRegexInPath) (\s@AWSManagedRulesATPRuleSet' {} a -> s {enableRegexInPath = a} :: AWSManagedRulesATPRuleSet)

-- | The criteria for inspecting login requests, used by the ATP rule group
-- to validate credentials usage.
aWSManagedRulesATPRuleSet_requestInspection :: Lens.Lens' AWSManagedRulesATPRuleSet (Prelude.Maybe RequestInspection)
aWSManagedRulesATPRuleSet_requestInspection = Lens.lens (\AWSManagedRulesATPRuleSet' {requestInspection} -> requestInspection) (\s@AWSManagedRulesATPRuleSet' {} a -> s {requestInspection = a} :: AWSManagedRulesATPRuleSet)

-- | The criteria for inspecting responses to login requests, used by the ATP
-- rule group to track login failure rates.
--
-- Response inspection is available only in web ACLs that protect Amazon
-- CloudFront distributions.
--
-- The ATP rule group evaluates the responses that your protected resources
-- send back to client login attempts, keeping count of successful and
-- failed attempts for each IP address and client session. Using this
-- information, the rule group labels and mitigates requests from client
-- sessions and IP addresses that have had too many failed login attempts
-- in a short amount of time.
aWSManagedRulesATPRuleSet_responseInspection :: Lens.Lens' AWSManagedRulesATPRuleSet (Prelude.Maybe ResponseInspection)
aWSManagedRulesATPRuleSet_responseInspection = Lens.lens (\AWSManagedRulesATPRuleSet' {responseInspection} -> responseInspection) (\s@AWSManagedRulesATPRuleSet' {} a -> s {responseInspection = a} :: AWSManagedRulesATPRuleSet)

-- | The path of the login endpoint for your application. For example, for
-- the URL @https:\/\/example.com\/web\/login@, you would provide the path
-- @\/web\/login@.
--
-- The rule group inspects only HTTP @POST@ requests to your specified
-- login endpoint.
aWSManagedRulesATPRuleSet_loginPath :: Lens.Lens' AWSManagedRulesATPRuleSet Prelude.Text
aWSManagedRulesATPRuleSet_loginPath = Lens.lens (\AWSManagedRulesATPRuleSet' {loginPath} -> loginPath) (\s@AWSManagedRulesATPRuleSet' {} a -> s {loginPath = a} :: AWSManagedRulesATPRuleSet)

instance Data.FromJSON AWSManagedRulesATPRuleSet where
  parseJSON =
    Data.withObject
      "AWSManagedRulesATPRuleSet"
      ( \x ->
          AWSManagedRulesATPRuleSet'
            Prelude.<$> (x Data..:? "EnableRegexInPath")
            Prelude.<*> (x Data..:? "RequestInspection")
            Prelude.<*> (x Data..:? "ResponseInspection")
            Prelude.<*> (x Data..: "LoginPath")
      )

instance Prelude.Hashable AWSManagedRulesATPRuleSet where
  hashWithSalt _salt AWSManagedRulesATPRuleSet' {..} =
    _salt
      `Prelude.hashWithSalt` enableRegexInPath
      `Prelude.hashWithSalt` requestInspection
      `Prelude.hashWithSalt` responseInspection
      `Prelude.hashWithSalt` loginPath

instance Prelude.NFData AWSManagedRulesATPRuleSet where
  rnf AWSManagedRulesATPRuleSet' {..} =
    Prelude.rnf enableRegexInPath
      `Prelude.seq` Prelude.rnf requestInspection
      `Prelude.seq` Prelude.rnf responseInspection
      `Prelude.seq` Prelude.rnf loginPath

instance Data.ToJSON AWSManagedRulesATPRuleSet where
  toJSON AWSManagedRulesATPRuleSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EnableRegexInPath" Data..=)
              Prelude.<$> enableRegexInPath,
            ("RequestInspection" Data..=)
              Prelude.<$> requestInspection,
            ("ResponseInspection" Data..=)
              Prelude.<$> responseInspection,
            Prelude.Just ("LoginPath" Data..= loginPath)
          ]
      )
