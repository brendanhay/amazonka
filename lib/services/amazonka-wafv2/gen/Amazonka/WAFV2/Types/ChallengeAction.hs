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
-- Module      : Amazonka.WAFV2.Types.ChallengeAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ChallengeAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.CustomRequestHandling

-- | Specifies that WAF should run a @Challenge@ check against the request to
-- verify that the request is coming from a legitimate client session:
--
-- -   If the request includes a valid, unexpired challenge token, WAF
--     applies any custom request handling and labels that you\'ve
--     configured and then allows the web request inspection to proceed to
--     the next rule, similar to a @CountAction@.
--
-- -   If the request doesn\'t include a valid, unexpired challenge token,
--     WAF discontinues the web ACL evaluation of the request and blocks it
--     from going to its intended destination.
--
--     WAF then generates a challenge response that it sends back to the
--     client, which includes the following:
--
--     -   The header @x-amzn-waf-action@ with a value of @challenge@.
--
--     -   The HTTP status code @202 Request Accepted@.
--
--     -   If the request contains an @Accept@ header with a value of
--         @text\/html@, the response includes a JavaScript page
--         interstitial with a challenge script.
--
--     Challenges run silent browser interrogations in the background, and
--     don\'t generally affect the end user experience.
--
--     A challenge enforces token acquisition using an interstitial
--     JavaScript challenge that inspects the client session for legitimate
--     behavior. The challenge blocks bots or at least increases the cost
--     of operating sophisticated bots.
--
--     After the client session successfully responds to the challenge, it
--     receives a new token from WAF, which the challenge script uses to
--     resubmit the original request.
--
-- You can configure the expiration time in the @ChallengeConfig@
-- @ImmunityTimeProperty@ setting at the rule and web ACL level. The rule
-- setting overrides the web ACL setting.
--
-- This action option is available for rules. It isn\'t available for web
-- ACL default actions.
--
-- /See:/ 'newChallengeAction' smart constructor.
data ChallengeAction = ChallengeAction'
  { -- | Defines custom handling for the web request, used when the challenge
    -- inspection determines that the request\'s token is valid and unexpired.
    --
    -- For information about customizing web requests and responses, see
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
    -- in the
    -- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
    customRequestHandling :: Prelude.Maybe CustomRequestHandling
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChallengeAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customRequestHandling', 'challengeAction_customRequestHandling' - Defines custom handling for the web request, used when the challenge
-- inspection determines that the request\'s token is valid and unexpired.
--
-- For information about customizing web requests and responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
newChallengeAction ::
  ChallengeAction
newChallengeAction =
  ChallengeAction'
    { customRequestHandling =
        Prelude.Nothing
    }

-- | Defines custom handling for the web request, used when the challenge
-- inspection determines that the request\'s token is valid and unexpired.
--
-- For information about customizing web requests and responses, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-custom-request-response.html Customizing web requests and responses in WAF>
-- in the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html WAF Developer Guide>.
challengeAction_customRequestHandling :: Lens.Lens' ChallengeAction (Prelude.Maybe CustomRequestHandling)
challengeAction_customRequestHandling = Lens.lens (\ChallengeAction' {customRequestHandling} -> customRequestHandling) (\s@ChallengeAction' {} a -> s {customRequestHandling = a} :: ChallengeAction)

instance Core.FromJSON ChallengeAction where
  parseJSON =
    Core.withObject
      "ChallengeAction"
      ( \x ->
          ChallengeAction'
            Prelude.<$> (x Core..:? "CustomRequestHandling")
      )

instance Prelude.Hashable ChallengeAction where
  hashWithSalt _salt ChallengeAction' {..} =
    _salt `Prelude.hashWithSalt` customRequestHandling

instance Prelude.NFData ChallengeAction where
  rnf ChallengeAction' {..} =
    Prelude.rnf customRequestHandling

instance Core.ToJSON ChallengeAction where
  toJSON ChallengeAction' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CustomRequestHandling" Core..=)
              Prelude.<$> customRequestHandling
          ]
      )
