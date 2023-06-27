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
-- Module      : Amazonka.CodeBuild.Types.WebhookFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.WebhookFilter where

import Amazonka.CodeBuild.Types.WebhookFilterType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A filter used to determine which webhooks trigger a build.
--
-- /See:/ 'newWebhookFilter' smart constructor.
data WebhookFilter = WebhookFilter'
  { -- | Used to indicate that the @pattern@ determines which webhook events do
    -- not trigger a build. If true, then a webhook event that does not match
    -- the @pattern@ triggers a build. If false, then a webhook event that
    -- matches the @pattern@ triggers a build.
    excludeMatchedPattern :: Prelude.Maybe Prelude.Bool,
    -- | The type of webhook filter. There are six webhook filter types: @EVENT@,
    -- @ACTOR_ACCOUNT_ID@, @HEAD_REF@, @BASE_REF@, @FILE_PATH@, and
    -- @COMMIT_MESSAGE@.
    --
    -- [EVENT]
    --     A webhook event triggers a build when the provided @pattern@ matches
    --     one of five event types: @PUSH@, @PULL_REQUEST_CREATED@,
    --     @PULL_REQUEST_UPDATED@, @PULL_REQUEST_REOPENED@, and
    --     @PULL_REQUEST_MERGED@. The @EVENT@ patterns are specified as a
    --     comma-separated string. For example,
    --     @PUSH, PULL_REQUEST_CREATED, PULL_REQUEST_UPDATED@ filters all push,
    --     pull request created, and pull request updated events.
    --
    --     The @PULL_REQUEST_REOPENED@ works with GitHub and GitHub Enterprise
    --     only.
    --
    -- [ACTOR_ACCOUNT_ID]
    --     A webhook event triggers a build when a GitHub, GitHub Enterprise,
    --     or Bitbucket account ID matches the regular expression @pattern@.
    --
    -- [HEAD_REF]
    --     A webhook event triggers a build when the head reference matches the
    --     regular expression @pattern@. For example,
    --     @refs\/heads\/branch-name@ and @refs\/tags\/tag-name@.
    --
    --     Works with GitHub and GitHub Enterprise push, GitHub and GitHub
    --     Enterprise pull request, Bitbucket push, and Bitbucket pull request
    --     events.
    --
    -- [BASE_REF]
    --     A webhook event triggers a build when the base reference matches the
    --     regular expression @pattern@. For example,
    --     @refs\/heads\/branch-name@.
    --
    --     Works with pull request events only.
    --
    -- [FILE_PATH]
    --     A webhook triggers a build when the path of a changed file matches
    --     the regular expression @pattern@.
    --
    --     Works with GitHub and Bitbucket events push and pull requests
    --     events. Also works with GitHub Enterprise push events, but does not
    --     work with GitHub Enterprise pull request events.
    --
    -- [COMMIT_MESSAGE]
    --     A webhook triggers a build when the head commit message matches the
    --     regular expression @pattern@.
    --
    --     Works with GitHub and Bitbucket events push and pull requests
    --     events. Also works with GitHub Enterprise push events, but does not
    --     work with GitHub Enterprise pull request events.
    type' :: WebhookFilterType,
    -- | For a @WebHookFilter@ that uses @EVENT@ type, a comma-separated string
    -- that specifies one or more events. For example, the webhook filter
    -- @PUSH, PULL_REQUEST_CREATED, PULL_REQUEST_UPDATED@ allows all push, pull
    -- request created, and pull request updated events to trigger a build.
    --
    -- For a @WebHookFilter@ that uses any of the other filter types, a regular
    -- expression pattern. For example, a @WebHookFilter@ that uses @HEAD_REF@
    -- for its @type@ and the pattern @^refs\/heads\/@ triggers a build when
    -- the head reference is a branch with a reference name
    -- @refs\/heads\/branch-name@.
    pattern' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WebhookFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludeMatchedPattern', 'webhookFilter_excludeMatchedPattern' - Used to indicate that the @pattern@ determines which webhook events do
-- not trigger a build. If true, then a webhook event that does not match
-- the @pattern@ triggers a build. If false, then a webhook event that
-- matches the @pattern@ triggers a build.
--
-- 'type'', 'webhookFilter_type' - The type of webhook filter. There are six webhook filter types: @EVENT@,
-- @ACTOR_ACCOUNT_ID@, @HEAD_REF@, @BASE_REF@, @FILE_PATH@, and
-- @COMMIT_MESSAGE@.
--
-- [EVENT]
--     A webhook event triggers a build when the provided @pattern@ matches
--     one of five event types: @PUSH@, @PULL_REQUEST_CREATED@,
--     @PULL_REQUEST_UPDATED@, @PULL_REQUEST_REOPENED@, and
--     @PULL_REQUEST_MERGED@. The @EVENT@ patterns are specified as a
--     comma-separated string. For example,
--     @PUSH, PULL_REQUEST_CREATED, PULL_REQUEST_UPDATED@ filters all push,
--     pull request created, and pull request updated events.
--
--     The @PULL_REQUEST_REOPENED@ works with GitHub and GitHub Enterprise
--     only.
--
-- [ACTOR_ACCOUNT_ID]
--     A webhook event triggers a build when a GitHub, GitHub Enterprise,
--     or Bitbucket account ID matches the regular expression @pattern@.
--
-- [HEAD_REF]
--     A webhook event triggers a build when the head reference matches the
--     regular expression @pattern@. For example,
--     @refs\/heads\/branch-name@ and @refs\/tags\/tag-name@.
--
--     Works with GitHub and GitHub Enterprise push, GitHub and GitHub
--     Enterprise pull request, Bitbucket push, and Bitbucket pull request
--     events.
--
-- [BASE_REF]
--     A webhook event triggers a build when the base reference matches the
--     regular expression @pattern@. For example,
--     @refs\/heads\/branch-name@.
--
--     Works with pull request events only.
--
-- [FILE_PATH]
--     A webhook triggers a build when the path of a changed file matches
--     the regular expression @pattern@.
--
--     Works with GitHub and Bitbucket events push and pull requests
--     events. Also works with GitHub Enterprise push events, but does not
--     work with GitHub Enterprise pull request events.
--
-- [COMMIT_MESSAGE]
--     A webhook triggers a build when the head commit message matches the
--     regular expression @pattern@.
--
--     Works with GitHub and Bitbucket events push and pull requests
--     events. Also works with GitHub Enterprise push events, but does not
--     work with GitHub Enterprise pull request events.
--
-- 'pattern'', 'webhookFilter_pattern' - For a @WebHookFilter@ that uses @EVENT@ type, a comma-separated string
-- that specifies one or more events. For example, the webhook filter
-- @PUSH, PULL_REQUEST_CREATED, PULL_REQUEST_UPDATED@ allows all push, pull
-- request created, and pull request updated events to trigger a build.
--
-- For a @WebHookFilter@ that uses any of the other filter types, a regular
-- expression pattern. For example, a @WebHookFilter@ that uses @HEAD_REF@
-- for its @type@ and the pattern @^refs\/heads\/@ triggers a build when
-- the head reference is a branch with a reference name
-- @refs\/heads\/branch-name@.
newWebhookFilter ::
  -- | 'type''
  WebhookFilterType ->
  -- | 'pattern''
  Prelude.Text ->
  WebhookFilter
newWebhookFilter pType_ pPattern_ =
  WebhookFilter'
    { excludeMatchedPattern =
        Prelude.Nothing,
      type' = pType_,
      pattern' = pPattern_
    }

-- | Used to indicate that the @pattern@ determines which webhook events do
-- not trigger a build. If true, then a webhook event that does not match
-- the @pattern@ triggers a build. If false, then a webhook event that
-- matches the @pattern@ triggers a build.
webhookFilter_excludeMatchedPattern :: Lens.Lens' WebhookFilter (Prelude.Maybe Prelude.Bool)
webhookFilter_excludeMatchedPattern = Lens.lens (\WebhookFilter' {excludeMatchedPattern} -> excludeMatchedPattern) (\s@WebhookFilter' {} a -> s {excludeMatchedPattern = a} :: WebhookFilter)

-- | The type of webhook filter. There are six webhook filter types: @EVENT@,
-- @ACTOR_ACCOUNT_ID@, @HEAD_REF@, @BASE_REF@, @FILE_PATH@, and
-- @COMMIT_MESSAGE@.
--
-- [EVENT]
--     A webhook event triggers a build when the provided @pattern@ matches
--     one of five event types: @PUSH@, @PULL_REQUEST_CREATED@,
--     @PULL_REQUEST_UPDATED@, @PULL_REQUEST_REOPENED@, and
--     @PULL_REQUEST_MERGED@. The @EVENT@ patterns are specified as a
--     comma-separated string. For example,
--     @PUSH, PULL_REQUEST_CREATED, PULL_REQUEST_UPDATED@ filters all push,
--     pull request created, and pull request updated events.
--
--     The @PULL_REQUEST_REOPENED@ works with GitHub and GitHub Enterprise
--     only.
--
-- [ACTOR_ACCOUNT_ID]
--     A webhook event triggers a build when a GitHub, GitHub Enterprise,
--     or Bitbucket account ID matches the regular expression @pattern@.
--
-- [HEAD_REF]
--     A webhook event triggers a build when the head reference matches the
--     regular expression @pattern@. For example,
--     @refs\/heads\/branch-name@ and @refs\/tags\/tag-name@.
--
--     Works with GitHub and GitHub Enterprise push, GitHub and GitHub
--     Enterprise pull request, Bitbucket push, and Bitbucket pull request
--     events.
--
-- [BASE_REF]
--     A webhook event triggers a build when the base reference matches the
--     regular expression @pattern@. For example,
--     @refs\/heads\/branch-name@.
--
--     Works with pull request events only.
--
-- [FILE_PATH]
--     A webhook triggers a build when the path of a changed file matches
--     the regular expression @pattern@.
--
--     Works with GitHub and Bitbucket events push and pull requests
--     events. Also works with GitHub Enterprise push events, but does not
--     work with GitHub Enterprise pull request events.
--
-- [COMMIT_MESSAGE]
--     A webhook triggers a build when the head commit message matches the
--     regular expression @pattern@.
--
--     Works with GitHub and Bitbucket events push and pull requests
--     events. Also works with GitHub Enterprise push events, but does not
--     work with GitHub Enterprise pull request events.
webhookFilter_type :: Lens.Lens' WebhookFilter WebhookFilterType
webhookFilter_type = Lens.lens (\WebhookFilter' {type'} -> type') (\s@WebhookFilter' {} a -> s {type' = a} :: WebhookFilter)

-- | For a @WebHookFilter@ that uses @EVENT@ type, a comma-separated string
-- that specifies one or more events. For example, the webhook filter
-- @PUSH, PULL_REQUEST_CREATED, PULL_REQUEST_UPDATED@ allows all push, pull
-- request created, and pull request updated events to trigger a build.
--
-- For a @WebHookFilter@ that uses any of the other filter types, a regular
-- expression pattern. For example, a @WebHookFilter@ that uses @HEAD_REF@
-- for its @type@ and the pattern @^refs\/heads\/@ triggers a build when
-- the head reference is a branch with a reference name
-- @refs\/heads\/branch-name@.
webhookFilter_pattern :: Lens.Lens' WebhookFilter Prelude.Text
webhookFilter_pattern = Lens.lens (\WebhookFilter' {pattern'} -> pattern') (\s@WebhookFilter' {} a -> s {pattern' = a} :: WebhookFilter)

instance Data.FromJSON WebhookFilter where
  parseJSON =
    Data.withObject
      "WebhookFilter"
      ( \x ->
          WebhookFilter'
            Prelude.<$> (x Data..:? "excludeMatchedPattern")
            Prelude.<*> (x Data..: "type")
            Prelude.<*> (x Data..: "pattern")
      )

instance Prelude.Hashable WebhookFilter where
  hashWithSalt _salt WebhookFilter' {..} =
    _salt
      `Prelude.hashWithSalt` excludeMatchedPattern
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` pattern'

instance Prelude.NFData WebhookFilter where
  rnf WebhookFilter' {..} =
    Prelude.rnf excludeMatchedPattern
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf pattern'

instance Data.ToJSON WebhookFilter where
  toJSON WebhookFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("excludeMatchedPattern" Data..=)
              Prelude.<$> excludeMatchedPattern,
            Prelude.Just ("type" Data..= type'),
            Prelude.Just ("pattern" Data..= pattern')
          ]
      )
