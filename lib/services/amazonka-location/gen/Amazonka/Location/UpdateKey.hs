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
-- Module      : Amazonka.Location.UpdateKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified properties of a given API key resource.
--
-- The API keys feature is in preview. We may add, change, or remove
-- features before announcing general availability. For more information,
-- see
-- <https://docs.aws.amazon.com/location/latest/developerguide/using-apikeys.html Using API keys>.
module Amazonka.Location.UpdateKey
  ( -- * Creating a Request
    UpdateKey (..),
    newUpdateKey,

    -- * Request Lenses
    updateKey_description,
    updateKey_expireTime,
    updateKey_forceUpdate,
    updateKey_noExpiry,
    updateKey_restrictions,
    updateKey_keyName,

    -- * Destructuring the Response
    UpdateKeyResponse (..),
    newUpdateKeyResponse,

    -- * Response Lenses
    updateKeyResponse_httpStatus,
    updateKeyResponse_keyArn,
    updateKeyResponse_keyName,
    updateKeyResponse_updateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateKey' smart constructor.
data UpdateKey = UpdateKey'
  { -- | Updates the description for the API key resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | Updates the timestamp for when the API key resource will expire in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    expireTime :: Prelude.Maybe Data.ISO8601,
    -- | The boolean flag to be included for updating @ExpireTime@ or
    -- @Restrictions@ details.
    --
    -- Must be set to @true@ to update an API key resource that has been used
    -- in the past 7 days.
    --
    -- @False@ if force update is not preferred
    --
    -- Default value: @False@
    forceUpdate :: Prelude.Maybe Prelude.Bool,
    -- | Whether the API key should expire. Set to @true@ to set the API key to
    -- have no expiration time.
    noExpiry :: Prelude.Maybe Prelude.Bool,
    -- | Updates the API key restrictions for the API key resource.
    restrictions :: Prelude.Maybe ApiKeyRestrictions,
    -- | The name of the API key resource to update.
    keyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateKey_description' - Updates the description for the API key resource.
--
-- 'expireTime', 'updateKey_expireTime' - Updates the timestamp for when the API key resource will expire in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- 'forceUpdate', 'updateKey_forceUpdate' - The boolean flag to be included for updating @ExpireTime@ or
-- @Restrictions@ details.
--
-- Must be set to @true@ to update an API key resource that has been used
-- in the past 7 days.
--
-- @False@ if force update is not preferred
--
-- Default value: @False@
--
-- 'noExpiry', 'updateKey_noExpiry' - Whether the API key should expire. Set to @true@ to set the API key to
-- have no expiration time.
--
-- 'restrictions', 'updateKey_restrictions' - Updates the API key restrictions for the API key resource.
--
-- 'keyName', 'updateKey_keyName' - The name of the API key resource to update.
newUpdateKey ::
  -- | 'keyName'
  Prelude.Text ->
  UpdateKey
newUpdateKey pKeyName_ =
  UpdateKey'
    { description = Prelude.Nothing,
      expireTime = Prelude.Nothing,
      forceUpdate = Prelude.Nothing,
      noExpiry = Prelude.Nothing,
      restrictions = Prelude.Nothing,
      keyName = pKeyName_
    }

-- | Updates the description for the API key resource.
updateKey_description :: Lens.Lens' UpdateKey (Prelude.Maybe Prelude.Text)
updateKey_description = Lens.lens (\UpdateKey' {description} -> description) (\s@UpdateKey' {} a -> s {description = a} :: UpdateKey)

-- | Updates the timestamp for when the API key resource will expire in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
updateKey_expireTime :: Lens.Lens' UpdateKey (Prelude.Maybe Prelude.UTCTime)
updateKey_expireTime = Lens.lens (\UpdateKey' {expireTime} -> expireTime) (\s@UpdateKey' {} a -> s {expireTime = a} :: UpdateKey) Prelude.. Lens.mapping Data._Time

-- | The boolean flag to be included for updating @ExpireTime@ or
-- @Restrictions@ details.
--
-- Must be set to @true@ to update an API key resource that has been used
-- in the past 7 days.
--
-- @False@ if force update is not preferred
--
-- Default value: @False@
updateKey_forceUpdate :: Lens.Lens' UpdateKey (Prelude.Maybe Prelude.Bool)
updateKey_forceUpdate = Lens.lens (\UpdateKey' {forceUpdate} -> forceUpdate) (\s@UpdateKey' {} a -> s {forceUpdate = a} :: UpdateKey)

-- | Whether the API key should expire. Set to @true@ to set the API key to
-- have no expiration time.
updateKey_noExpiry :: Lens.Lens' UpdateKey (Prelude.Maybe Prelude.Bool)
updateKey_noExpiry = Lens.lens (\UpdateKey' {noExpiry} -> noExpiry) (\s@UpdateKey' {} a -> s {noExpiry = a} :: UpdateKey)

-- | Updates the API key restrictions for the API key resource.
updateKey_restrictions :: Lens.Lens' UpdateKey (Prelude.Maybe ApiKeyRestrictions)
updateKey_restrictions = Lens.lens (\UpdateKey' {restrictions} -> restrictions) (\s@UpdateKey' {} a -> s {restrictions = a} :: UpdateKey)

-- | The name of the API key resource to update.
updateKey_keyName :: Lens.Lens' UpdateKey Prelude.Text
updateKey_keyName = Lens.lens (\UpdateKey' {keyName} -> keyName) (\s@UpdateKey' {} a -> s {keyName = a} :: UpdateKey)

instance Core.AWSRequest UpdateKey where
  type AWSResponse UpdateKey = UpdateKeyResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateKeyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "KeyArn")
            Prelude.<*> (x Data..:> "KeyName")
            Prelude.<*> (x Data..:> "UpdateTime")
      )

instance Prelude.Hashable UpdateKey where
  hashWithSalt _salt UpdateKey' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` expireTime
      `Prelude.hashWithSalt` forceUpdate
      `Prelude.hashWithSalt` noExpiry
      `Prelude.hashWithSalt` restrictions
      `Prelude.hashWithSalt` keyName

instance Prelude.NFData UpdateKey where
  rnf UpdateKey' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf expireTime
      `Prelude.seq` Prelude.rnf forceUpdate
      `Prelude.seq` Prelude.rnf noExpiry
      `Prelude.seq` Prelude.rnf restrictions
      `Prelude.seq` Prelude.rnf keyName

instance Data.ToHeaders UpdateKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateKey where
  toJSON UpdateKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("ExpireTime" Data..=) Prelude.<$> expireTime,
            ("ForceUpdate" Data..=) Prelude.<$> forceUpdate,
            ("NoExpiry" Data..=) Prelude.<$> noExpiry,
            ("Restrictions" Data..=) Prelude.<$> restrictions
          ]
      )

instance Data.ToPath UpdateKey where
  toPath UpdateKey' {..} =
    Prelude.mconcat
      ["/metadata/v0/keys/", Data.toBS keyName]

instance Data.ToQuery UpdateKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateKeyResponse' smart constructor.
data UpdateKeyResponse = UpdateKeyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) for the API key resource. Used when you
    -- need to specify a resource across all Amazon Web Services.
    --
    -- -   Format example: @arn:aws:geo:region:account-id:key\/ExampleKey@
    keyArn :: Prelude.Text,
    -- | The name of the API key resource.
    keyName :: Prelude.Text,
    -- | The timestamp for when the API key resource was last updated in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    updateTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateKeyResponse_httpStatus' - The response's http status code.
--
-- 'keyArn', 'updateKeyResponse_keyArn' - The Amazon Resource Name (ARN) for the API key resource. Used when you
-- need to specify a resource across all Amazon Web Services.
--
-- -   Format example: @arn:aws:geo:region:account-id:key\/ExampleKey@
--
-- 'keyName', 'updateKeyResponse_keyName' - The name of the API key resource.
--
-- 'updateTime', 'updateKeyResponse_updateTime' - The timestamp for when the API key resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
newUpdateKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'keyArn'
  Prelude.Text ->
  -- | 'keyName'
  Prelude.Text ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  UpdateKeyResponse
newUpdateKeyResponse
  pHttpStatus_
  pKeyArn_
  pKeyName_
  pUpdateTime_ =
    UpdateKeyResponse'
      { httpStatus = pHttpStatus_,
        keyArn = pKeyArn_,
        keyName = pKeyName_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | The response's http status code.
updateKeyResponse_httpStatus :: Lens.Lens' UpdateKeyResponse Prelude.Int
updateKeyResponse_httpStatus = Lens.lens (\UpdateKeyResponse' {httpStatus} -> httpStatus) (\s@UpdateKeyResponse' {} a -> s {httpStatus = a} :: UpdateKeyResponse)

-- | The Amazon Resource Name (ARN) for the API key resource. Used when you
-- need to specify a resource across all Amazon Web Services.
--
-- -   Format example: @arn:aws:geo:region:account-id:key\/ExampleKey@
updateKeyResponse_keyArn :: Lens.Lens' UpdateKeyResponse Prelude.Text
updateKeyResponse_keyArn = Lens.lens (\UpdateKeyResponse' {keyArn} -> keyArn) (\s@UpdateKeyResponse' {} a -> s {keyArn = a} :: UpdateKeyResponse)

-- | The name of the API key resource.
updateKeyResponse_keyName :: Lens.Lens' UpdateKeyResponse Prelude.Text
updateKeyResponse_keyName = Lens.lens (\UpdateKeyResponse' {keyName} -> keyName) (\s@UpdateKeyResponse' {} a -> s {keyName = a} :: UpdateKeyResponse)

-- | The timestamp for when the API key resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
updateKeyResponse_updateTime :: Lens.Lens' UpdateKeyResponse Prelude.UTCTime
updateKeyResponse_updateTime = Lens.lens (\UpdateKeyResponse' {updateTime} -> updateTime) (\s@UpdateKeyResponse' {} a -> s {updateTime = a} :: UpdateKeyResponse) Prelude.. Data._Time

instance Prelude.NFData UpdateKeyResponse where
  rnf UpdateKeyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf keyArn
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf updateTime
