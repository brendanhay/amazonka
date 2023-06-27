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
-- Module      : Amazonka.Location.Types.ApiKeyRestrictions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.ApiKeyRestrictions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | API Restrictions on the allowed actions, resources, and referers for an
-- API key resource.
--
-- /See:/ 'newApiKeyRestrictions' smart constructor.
data ApiKeyRestrictions = ApiKeyRestrictions'
  { -- | An optional list of allowed HTTP referers for which requests must
    -- originate from. Requests using this API key from other domains will not
    -- be allowed.
    --
    -- Requirements:
    --
    -- -   Contain only alphanumeric characters (A–Z, a–z, 0–9) or any symbols
    --     in this list @$\\-._+!*\`(),;\/?:\@=&@
    --
    -- -   May contain a percent (%) if followed by 2 hexadecimal digits (A-F,
    --     a-f, 0-9); this is used for URL encoding purposes.
    --
    -- -   May contain wildcard characters question mark (?) and asterisk (*).
    --
    --     Question mark (?) will replace any single character (including
    --     hexadecimal digits).
    --
    --     Asterisk (*) will replace any multiple characters (including
    --     multiple hexadecimal digits).
    --
    -- -   No spaces allowed. For example, @https:\/\/example.com@.
    allowReferers :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | A list of allowed actions that an API key resource grants permissions to
    -- perform
    --
    -- Currently, the only valid action is @geo:GetMap*@ as an input to the
    -- list. For example, @[\"geo:GetMap*\"]@ is valid but
    -- @[\"geo:GetMapTile\"]@ is not.
    allowActions :: Prelude.NonEmpty Prelude.Text,
    -- | A list of allowed resource ARNs that a API key bearer can perform
    -- actions on
    --
    -- For more information about ARN format, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
    --
    -- In this preview, you can allow only map resources.
    --
    -- Requirements:
    --
    -- -   Must be prefixed with @arn@.
    --
    -- -   @partition@ and @service@ must not be empty and should begin with
    --     only alphanumeric characters (A–Z, a–z, 0–9) and contain only
    --     alphanumeric numbers, hyphens (-) and periods (.).
    --
    -- -   @region@ and @account-id@ can be empty or should begin with only
    --     alphanumeric characters (A–Z, a–z, 0–9) and contain only
    --     alphanumeric numbers, hyphens (-) and periods (.).
    --
    -- -   @resource-id@ can begin with any character except for forward slash
    --     (\/) and contain any characters after, including forward slashes to
    --     form a path.
    --
    --     @resource-id@ can also include wildcard characters, denoted by an
    --     asterisk (*).
    --
    -- -   @arn@, @partition@, @service@, @region@, @account-id@ and
    --     @resource-id@ must be delimited by a colon (:).
    --
    -- -   No spaces allowed. For example,
    --     @arn:aws:geo:region:@/@account-id@/@:map\/ExampleMap*@.
    allowResources :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApiKeyRestrictions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowReferers', 'apiKeyRestrictions_allowReferers' - An optional list of allowed HTTP referers for which requests must
-- originate from. Requests using this API key from other domains will not
-- be allowed.
--
-- Requirements:
--
-- -   Contain only alphanumeric characters (A–Z, a–z, 0–9) or any symbols
--     in this list @$\\-._+!*\`(),;\/?:\@=&@
--
-- -   May contain a percent (%) if followed by 2 hexadecimal digits (A-F,
--     a-f, 0-9); this is used for URL encoding purposes.
--
-- -   May contain wildcard characters question mark (?) and asterisk (*).
--
--     Question mark (?) will replace any single character (including
--     hexadecimal digits).
--
--     Asterisk (*) will replace any multiple characters (including
--     multiple hexadecimal digits).
--
-- -   No spaces allowed. For example, @https:\/\/example.com@.
--
-- 'allowActions', 'apiKeyRestrictions_allowActions' - A list of allowed actions that an API key resource grants permissions to
-- perform
--
-- Currently, the only valid action is @geo:GetMap*@ as an input to the
-- list. For example, @[\"geo:GetMap*\"]@ is valid but
-- @[\"geo:GetMapTile\"]@ is not.
--
-- 'allowResources', 'apiKeyRestrictions_allowResources' - A list of allowed resource ARNs that a API key bearer can perform
-- actions on
--
-- For more information about ARN format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
--
-- In this preview, you can allow only map resources.
--
-- Requirements:
--
-- -   Must be prefixed with @arn@.
--
-- -   @partition@ and @service@ must not be empty and should begin with
--     only alphanumeric characters (A–Z, a–z, 0–9) and contain only
--     alphanumeric numbers, hyphens (-) and periods (.).
--
-- -   @region@ and @account-id@ can be empty or should begin with only
--     alphanumeric characters (A–Z, a–z, 0–9) and contain only
--     alphanumeric numbers, hyphens (-) and periods (.).
--
-- -   @resource-id@ can begin with any character except for forward slash
--     (\/) and contain any characters after, including forward slashes to
--     form a path.
--
--     @resource-id@ can also include wildcard characters, denoted by an
--     asterisk (*).
--
-- -   @arn@, @partition@, @service@, @region@, @account-id@ and
--     @resource-id@ must be delimited by a colon (:).
--
-- -   No spaces allowed. For example,
--     @arn:aws:geo:region:@/@account-id@/@:map\/ExampleMap*@.
newApiKeyRestrictions ::
  -- | 'allowActions'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'allowResources'
  Prelude.NonEmpty Prelude.Text ->
  ApiKeyRestrictions
newApiKeyRestrictions pAllowActions_ pAllowResources_ =
  ApiKeyRestrictions'
    { allowReferers =
        Prelude.Nothing,
      allowActions = Lens.coerced Lens.# pAllowActions_,
      allowResources =
        Lens.coerced Lens.# pAllowResources_
    }

-- | An optional list of allowed HTTP referers for which requests must
-- originate from. Requests using this API key from other domains will not
-- be allowed.
--
-- Requirements:
--
-- -   Contain only alphanumeric characters (A–Z, a–z, 0–9) or any symbols
--     in this list @$\\-._+!*\`(),;\/?:\@=&@
--
-- -   May contain a percent (%) if followed by 2 hexadecimal digits (A-F,
--     a-f, 0-9); this is used for URL encoding purposes.
--
-- -   May contain wildcard characters question mark (?) and asterisk (*).
--
--     Question mark (?) will replace any single character (including
--     hexadecimal digits).
--
--     Asterisk (*) will replace any multiple characters (including
--     multiple hexadecimal digits).
--
-- -   No spaces allowed. For example, @https:\/\/example.com@.
apiKeyRestrictions_allowReferers :: Lens.Lens' ApiKeyRestrictions (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
apiKeyRestrictions_allowReferers = Lens.lens (\ApiKeyRestrictions' {allowReferers} -> allowReferers) (\s@ApiKeyRestrictions' {} a -> s {allowReferers = a} :: ApiKeyRestrictions) Prelude.. Lens.mapping Lens.coerced

-- | A list of allowed actions that an API key resource grants permissions to
-- perform
--
-- Currently, the only valid action is @geo:GetMap*@ as an input to the
-- list. For example, @[\"geo:GetMap*\"]@ is valid but
-- @[\"geo:GetMapTile\"]@ is not.
apiKeyRestrictions_allowActions :: Lens.Lens' ApiKeyRestrictions (Prelude.NonEmpty Prelude.Text)
apiKeyRestrictions_allowActions = Lens.lens (\ApiKeyRestrictions' {allowActions} -> allowActions) (\s@ApiKeyRestrictions' {} a -> s {allowActions = a} :: ApiKeyRestrictions) Prelude.. Lens.coerced

-- | A list of allowed resource ARNs that a API key bearer can perform
-- actions on
--
-- For more information about ARN format, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
--
-- In this preview, you can allow only map resources.
--
-- Requirements:
--
-- -   Must be prefixed with @arn@.
--
-- -   @partition@ and @service@ must not be empty and should begin with
--     only alphanumeric characters (A–Z, a–z, 0–9) and contain only
--     alphanumeric numbers, hyphens (-) and periods (.).
--
-- -   @region@ and @account-id@ can be empty or should begin with only
--     alphanumeric characters (A–Z, a–z, 0–9) and contain only
--     alphanumeric numbers, hyphens (-) and periods (.).
--
-- -   @resource-id@ can begin with any character except for forward slash
--     (\/) and contain any characters after, including forward slashes to
--     form a path.
--
--     @resource-id@ can also include wildcard characters, denoted by an
--     asterisk (*).
--
-- -   @arn@, @partition@, @service@, @region@, @account-id@ and
--     @resource-id@ must be delimited by a colon (:).
--
-- -   No spaces allowed. For example,
--     @arn:aws:geo:region:@/@account-id@/@:map\/ExampleMap*@.
apiKeyRestrictions_allowResources :: Lens.Lens' ApiKeyRestrictions (Prelude.NonEmpty Prelude.Text)
apiKeyRestrictions_allowResources = Lens.lens (\ApiKeyRestrictions' {allowResources} -> allowResources) (\s@ApiKeyRestrictions' {} a -> s {allowResources = a} :: ApiKeyRestrictions) Prelude.. Lens.coerced

instance Data.FromJSON ApiKeyRestrictions where
  parseJSON =
    Data.withObject
      "ApiKeyRestrictions"
      ( \x ->
          ApiKeyRestrictions'
            Prelude.<$> (x Data..:? "AllowReferers")
            Prelude.<*> (x Data..: "AllowActions")
            Prelude.<*> (x Data..: "AllowResources")
      )

instance Prelude.Hashable ApiKeyRestrictions where
  hashWithSalt _salt ApiKeyRestrictions' {..} =
    _salt
      `Prelude.hashWithSalt` allowReferers
      `Prelude.hashWithSalt` allowActions
      `Prelude.hashWithSalt` allowResources

instance Prelude.NFData ApiKeyRestrictions where
  rnf ApiKeyRestrictions' {..} =
    Prelude.rnf allowReferers
      `Prelude.seq` Prelude.rnf allowActions
      `Prelude.seq` Prelude.rnf allowResources

instance Data.ToJSON ApiKeyRestrictions where
  toJSON ApiKeyRestrictions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowReferers" Data..=) Prelude.<$> allowReferers,
            Prelude.Just ("AllowActions" Data..= allowActions),
            Prelude.Just
              ("AllowResources" Data..= allowResources)
          ]
      )
