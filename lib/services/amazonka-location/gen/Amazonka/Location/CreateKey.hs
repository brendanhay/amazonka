{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Location.CreateKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an API key resource in your Amazon Web Services account, which
-- lets you grant @geo:GetMap*@ actions for Amazon Location Map resources
-- to the API key bearer.
--
-- The API keys feature is in preview. We may add, change, or remove
-- features before announcing general availability. For more information,
-- see
-- <https://docs.aws.amazon.com/location/latest/developerguide/using-apikeys.html Using API keys>.
module Amazonka.Location.CreateKey
  ( -- * Creating a Request
    CreateKey (..),
    newCreateKey,

    -- * Request Lenses
    createKey_description,
    createKey_expireTime,
    createKey_noExpiry,
    createKey_tags,
    createKey_keyName,
    createKey_restrictions,

    -- * Destructuring the Response
    CreateKeyResponse (..),
    newCreateKeyResponse,

    -- * Response Lenses
    createKeyResponse_httpStatus,
    createKeyResponse_createTime,
    createKeyResponse_key,
    createKeyResponse_keyArn,
    createKeyResponse_keyName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateKey' smart constructor.
data CreateKey = CreateKey'
  { -- | An optional description for the API key resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | The optional timestamp for when the API key resource will expire in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@. One of @NoExpiry@ or @ExpireTime@
    -- must be set.
    expireTime :: Prelude.Maybe Data.ISO8601,
    -- | Optionally set to @true@ to set no expiration time for the API key. One
    -- of @NoExpiry@ or @ExpireTime@ must be set.
    noExpiry :: Prelude.Maybe Prelude.Bool,
    -- | Applies one or more tags to the map resource. A tag is a key-value pair
    -- that helps manage, identify, search, and filter your resources by
    -- labelling them.
    --
    -- Format: @\"key\" : \"value\"@
    --
    -- Restrictions:
    --
    -- -   Maximum 50 tags per resource
    --
    -- -   Each resource tag must be unique with a maximum of one value.
    --
    -- -   Maximum key length: 128 Unicode characters in UTF-8
    --
    -- -   Maximum value length: 256 Unicode characters in UTF-8
    --
    -- -   Can use alphanumeric characters (A–Z, a–z, 0–9), and the following
    --     characters: + - = . _ : \/ \@.
    --
    -- -   Cannot use \"aws:\" as a prefix for a key.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A custom name for the API key resource.
    --
    -- Requirements:
    --
    -- -   Contain only alphanumeric characters (A–Z, a–z, 0–9), hyphens (-),
    --     periods (.), and underscores (_).
    --
    -- -   Must be a unique API key name.
    --
    -- -   No spaces allowed. For example, @ExampleAPIKey@.
    keyName :: Prelude.Text,
    -- | The API key restrictions for the API key resource.
    restrictions :: ApiKeyRestrictions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createKey_description' - An optional description for the API key resource.
--
-- 'expireTime', 'createKey_expireTime' - The optional timestamp for when the API key resource will expire in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@. One of @NoExpiry@ or @ExpireTime@
-- must be set.
--
-- 'noExpiry', 'createKey_noExpiry' - Optionally set to @true@ to set no expiration time for the API key. One
-- of @NoExpiry@ or @ExpireTime@ must be set.
--
-- 'tags', 'createKey_tags' - Applies one or more tags to the map resource. A tag is a key-value pair
-- that helps manage, identify, search, and filter your resources by
-- labelling them.
--
-- Format: @\"key\" : \"value\"@
--
-- Restrictions:
--
-- -   Maximum 50 tags per resource
--
-- -   Each resource tag must be unique with a maximum of one value.
--
-- -   Maximum key length: 128 Unicode characters in UTF-8
--
-- -   Maximum value length: 256 Unicode characters in UTF-8
--
-- -   Can use alphanumeric characters (A–Z, a–z, 0–9), and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Cannot use \"aws:\" as a prefix for a key.
--
-- 'keyName', 'createKey_keyName' - A custom name for the API key resource.
--
-- Requirements:
--
-- -   Contain only alphanumeric characters (A–Z, a–z, 0–9), hyphens (-),
--     periods (.), and underscores (_).
--
-- -   Must be a unique API key name.
--
-- -   No spaces allowed. For example, @ExampleAPIKey@.
--
-- 'restrictions', 'createKey_restrictions' - The API key restrictions for the API key resource.
newCreateKey ::
  -- | 'keyName'
  Prelude.Text ->
  -- | 'restrictions'
  ApiKeyRestrictions ->
  CreateKey
newCreateKey pKeyName_ pRestrictions_ =
  CreateKey'
    { description = Prelude.Nothing,
      expireTime = Prelude.Nothing,
      noExpiry = Prelude.Nothing,
      tags = Prelude.Nothing,
      keyName = pKeyName_,
      restrictions = pRestrictions_
    }

-- | An optional description for the API key resource.
createKey_description :: Lens.Lens' CreateKey (Prelude.Maybe Prelude.Text)
createKey_description = Lens.lens (\CreateKey' {description} -> description) (\s@CreateKey' {} a -> s {description = a} :: CreateKey)

-- | The optional timestamp for when the API key resource will expire in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@. One of @NoExpiry@ or @ExpireTime@
-- must be set.
createKey_expireTime :: Lens.Lens' CreateKey (Prelude.Maybe Prelude.UTCTime)
createKey_expireTime = Lens.lens (\CreateKey' {expireTime} -> expireTime) (\s@CreateKey' {} a -> s {expireTime = a} :: CreateKey) Prelude.. Lens.mapping Data._Time

-- | Optionally set to @true@ to set no expiration time for the API key. One
-- of @NoExpiry@ or @ExpireTime@ must be set.
createKey_noExpiry :: Lens.Lens' CreateKey (Prelude.Maybe Prelude.Bool)
createKey_noExpiry = Lens.lens (\CreateKey' {noExpiry} -> noExpiry) (\s@CreateKey' {} a -> s {noExpiry = a} :: CreateKey)

-- | Applies one or more tags to the map resource. A tag is a key-value pair
-- that helps manage, identify, search, and filter your resources by
-- labelling them.
--
-- Format: @\"key\" : \"value\"@
--
-- Restrictions:
--
-- -   Maximum 50 tags per resource
--
-- -   Each resource tag must be unique with a maximum of one value.
--
-- -   Maximum key length: 128 Unicode characters in UTF-8
--
-- -   Maximum value length: 256 Unicode characters in UTF-8
--
-- -   Can use alphanumeric characters (A–Z, a–z, 0–9), and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Cannot use \"aws:\" as a prefix for a key.
createKey_tags :: Lens.Lens' CreateKey (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createKey_tags = Lens.lens (\CreateKey' {tags} -> tags) (\s@CreateKey' {} a -> s {tags = a} :: CreateKey) Prelude.. Lens.mapping Lens.coerced

-- | A custom name for the API key resource.
--
-- Requirements:
--
-- -   Contain only alphanumeric characters (A–Z, a–z, 0–9), hyphens (-),
--     periods (.), and underscores (_).
--
-- -   Must be a unique API key name.
--
-- -   No spaces allowed. For example, @ExampleAPIKey@.
createKey_keyName :: Lens.Lens' CreateKey Prelude.Text
createKey_keyName = Lens.lens (\CreateKey' {keyName} -> keyName) (\s@CreateKey' {} a -> s {keyName = a} :: CreateKey)

-- | The API key restrictions for the API key resource.
createKey_restrictions :: Lens.Lens' CreateKey ApiKeyRestrictions
createKey_restrictions = Lens.lens (\CreateKey' {restrictions} -> restrictions) (\s@CreateKey' {} a -> s {restrictions = a} :: CreateKey)

instance Core.AWSRequest CreateKey where
  type AWSResponse CreateKey = CreateKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CreateTime")
            Prelude.<*> (x Data..:> "Key")
            Prelude.<*> (x Data..:> "KeyArn")
            Prelude.<*> (x Data..:> "KeyName")
      )

instance Prelude.Hashable CreateKey where
  hashWithSalt _salt CreateKey' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` expireTime
      `Prelude.hashWithSalt` noExpiry
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` restrictions

instance Prelude.NFData CreateKey where
  rnf CreateKey' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf expireTime
      `Prelude.seq` Prelude.rnf noExpiry
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf restrictions

instance Data.ToHeaders CreateKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateKey where
  toJSON CreateKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("ExpireTime" Data..=) Prelude.<$> expireTime,
            ("NoExpiry" Data..=) Prelude.<$> noExpiry,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("KeyName" Data..= keyName),
            Prelude.Just ("Restrictions" Data..= restrictions)
          ]
      )

instance Data.ToPath CreateKey where
  toPath = Prelude.const "/metadata/v0/keys"

instance Data.ToQuery CreateKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateKeyResponse' smart constructor.
data CreateKeyResponse = CreateKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The timestamp for when the API key resource was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    createTime :: Data.ISO8601,
    -- | The key value\/string of an API key. This value is used when making API
    -- calls to authorize the call. For example, see
    -- <https://docs.aws.amazon.com/location/latest/APIReference/API_GetMapGlyphs.html GetMapGlyphs>.
    key :: Data.Sensitive Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the API key resource. Used when you
    -- need to specify a resource across all Amazon Web Services.
    --
    -- -   Format example: @arn:aws:geo:region:account-id:key\/ExampleKey@
    keyArn :: Prelude.Text,
    -- | The name of the API key resource.
    keyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createKeyResponse_httpStatus' - The response's http status code.
--
-- 'createTime', 'createKeyResponse_createTime' - The timestamp for when the API key resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- 'key', 'createKeyResponse_key' - The key value\/string of an API key. This value is used when making API
-- calls to authorize the call. For example, see
-- <https://docs.aws.amazon.com/location/latest/APIReference/API_GetMapGlyphs.html GetMapGlyphs>.
--
-- 'keyArn', 'createKeyResponse_keyArn' - The Amazon Resource Name (ARN) for the API key resource. Used when you
-- need to specify a resource across all Amazon Web Services.
--
-- -   Format example: @arn:aws:geo:region:account-id:key\/ExampleKey@
--
-- 'keyName', 'createKeyResponse_keyName' - The name of the API key resource.
newCreateKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'key'
  Prelude.Text ->
  -- | 'keyArn'
  Prelude.Text ->
  -- | 'keyName'
  Prelude.Text ->
  CreateKeyResponse
newCreateKeyResponse
  pHttpStatus_
  pCreateTime_
  pKey_
  pKeyArn_
  pKeyName_ =
    CreateKeyResponse'
      { httpStatus = pHttpStatus_,
        createTime = Data._Time Lens.# pCreateTime_,
        key = Data._Sensitive Lens.# pKey_,
        keyArn = pKeyArn_,
        keyName = pKeyName_
      }

-- | The response's http status code.
createKeyResponse_httpStatus :: Lens.Lens' CreateKeyResponse Prelude.Int
createKeyResponse_httpStatus = Lens.lens (\CreateKeyResponse' {httpStatus} -> httpStatus) (\s@CreateKeyResponse' {} a -> s {httpStatus = a} :: CreateKeyResponse)

-- | The timestamp for when the API key resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
createKeyResponse_createTime :: Lens.Lens' CreateKeyResponse Prelude.UTCTime
createKeyResponse_createTime = Lens.lens (\CreateKeyResponse' {createTime} -> createTime) (\s@CreateKeyResponse' {} a -> s {createTime = a} :: CreateKeyResponse) Prelude.. Data._Time

-- | The key value\/string of an API key. This value is used when making API
-- calls to authorize the call. For example, see
-- <https://docs.aws.amazon.com/location/latest/APIReference/API_GetMapGlyphs.html GetMapGlyphs>.
createKeyResponse_key :: Lens.Lens' CreateKeyResponse Prelude.Text
createKeyResponse_key = Lens.lens (\CreateKeyResponse' {key} -> key) (\s@CreateKeyResponse' {} a -> s {key = a} :: CreateKeyResponse) Prelude.. Data._Sensitive

-- | The Amazon Resource Name (ARN) for the API key resource. Used when you
-- need to specify a resource across all Amazon Web Services.
--
-- -   Format example: @arn:aws:geo:region:account-id:key\/ExampleKey@
createKeyResponse_keyArn :: Lens.Lens' CreateKeyResponse Prelude.Text
createKeyResponse_keyArn = Lens.lens (\CreateKeyResponse' {keyArn} -> keyArn) (\s@CreateKeyResponse' {} a -> s {keyArn = a} :: CreateKeyResponse)

-- | The name of the API key resource.
createKeyResponse_keyName :: Lens.Lens' CreateKeyResponse Prelude.Text
createKeyResponse_keyName = Lens.lens (\CreateKeyResponse' {keyName} -> keyName) (\s@CreateKeyResponse' {} a -> s {keyName = a} :: CreateKeyResponse)

instance Prelude.NFData CreateKeyResponse where
  rnf CreateKeyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf keyArn
      `Prelude.seq` Prelude.rnf keyName
