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
-- Module      : Amazonka.Location.DescribeKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the API key resource details.
--
-- The API keys feature is in preview. We may add, change, or remove
-- features before announcing general availability. For more information,
-- see
-- <https://docs.aws.amazon.com/location/latest/developerguide/using-apikeys.html Using API keys>.
module Amazonka.Location.DescribeKey
  ( -- * Creating a Request
    DescribeKey (..),
    newDescribeKey,

    -- * Request Lenses
    describeKey_keyName,

    -- * Destructuring the Response
    DescribeKeyResponse (..),
    newDescribeKeyResponse,

    -- * Response Lenses
    describeKeyResponse_description,
    describeKeyResponse_tags,
    describeKeyResponse_httpStatus,
    describeKeyResponse_createTime,
    describeKeyResponse_expireTime,
    describeKeyResponse_key,
    describeKeyResponse_keyArn,
    describeKeyResponse_keyName,
    describeKeyResponse_restrictions,
    describeKeyResponse_updateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeKey' smart constructor.
data DescribeKey = DescribeKey'
  { -- | The name of the API key resource.
    keyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyName', 'describeKey_keyName' - The name of the API key resource.
newDescribeKey ::
  -- | 'keyName'
  Prelude.Text ->
  DescribeKey
newDescribeKey pKeyName_ =
  DescribeKey' {keyName = pKeyName_}

-- | The name of the API key resource.
describeKey_keyName :: Lens.Lens' DescribeKey Prelude.Text
describeKey_keyName = Lens.lens (\DescribeKey' {keyName} -> keyName) (\s@DescribeKey' {} a -> s {keyName = a} :: DescribeKey)

instance Core.AWSRequest DescribeKey where
  type AWSResponse DescribeKey = DescribeKeyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeKeyResponse'
            Prelude.<$> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CreateTime")
            Prelude.<*> (x Data..:> "ExpireTime")
            Prelude.<*> (x Data..:> "Key")
            Prelude.<*> (x Data..:> "KeyArn")
            Prelude.<*> (x Data..:> "KeyName")
            Prelude.<*> (x Data..:> "Restrictions")
            Prelude.<*> (x Data..:> "UpdateTime")
      )

instance Prelude.Hashable DescribeKey where
  hashWithSalt _salt DescribeKey' {..} =
    _salt `Prelude.hashWithSalt` keyName

instance Prelude.NFData DescribeKey where
  rnf DescribeKey' {..} = Prelude.rnf keyName

instance Data.ToHeaders DescribeKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeKey where
  toPath DescribeKey' {..} =
    Prelude.mconcat
      ["/metadata/v0/keys/", Data.toBS keyName]

instance Data.ToQuery DescribeKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeKeyResponse' smart constructor.
data DescribeKeyResponse = DescribeKeyResponse'
  { -- | The optional description for the API key resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | Tags associated with the API key resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The timestamp for when the API key resource was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    createTime :: Data.ISO8601,
    -- | The timestamp for when the API key resource will expire in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    expireTime :: Data.ISO8601,
    -- | The key value\/string of an API key.
    key :: Data.Sensitive Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the API key resource. Used when you
    -- need to specify a resource across all Amazon Web Services.
    --
    -- -   Format example: @arn:aws:geo:region:account-id:key\/ExampleKey@
    keyArn :: Prelude.Text,
    -- | The name of the API key resource.
    keyName :: Prelude.Text,
    restrictions :: ApiKeyRestrictions,
    -- | The timestamp for when the API key resource was last updated in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    updateTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'describeKeyResponse_description' - The optional description for the API key resource.
--
-- 'tags', 'describeKeyResponse_tags' - Tags associated with the API key resource.
--
-- 'httpStatus', 'describeKeyResponse_httpStatus' - The response's http status code.
--
-- 'createTime', 'describeKeyResponse_createTime' - The timestamp for when the API key resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- 'expireTime', 'describeKeyResponse_expireTime' - The timestamp for when the API key resource will expire in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- 'key', 'describeKeyResponse_key' - The key value\/string of an API key.
--
-- 'keyArn', 'describeKeyResponse_keyArn' - The Amazon Resource Name (ARN) for the API key resource. Used when you
-- need to specify a resource across all Amazon Web Services.
--
-- -   Format example: @arn:aws:geo:region:account-id:key\/ExampleKey@
--
-- 'keyName', 'describeKeyResponse_keyName' - The name of the API key resource.
--
-- 'restrictions', 'describeKeyResponse_restrictions' - Undocumented member.
--
-- 'updateTime', 'describeKeyResponse_updateTime' - The timestamp for when the API key resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
newDescribeKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'expireTime'
  Prelude.UTCTime ->
  -- | 'key'
  Prelude.Text ->
  -- | 'keyArn'
  Prelude.Text ->
  -- | 'keyName'
  Prelude.Text ->
  -- | 'restrictions'
  ApiKeyRestrictions ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  DescribeKeyResponse
newDescribeKeyResponse
  pHttpStatus_
  pCreateTime_
  pExpireTime_
  pKey_
  pKeyArn_
  pKeyName_
  pRestrictions_
  pUpdateTime_ =
    DescribeKeyResponse'
      { description = Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        createTime = Data._Time Lens.# pCreateTime_,
        expireTime = Data._Time Lens.# pExpireTime_,
        key = Data._Sensitive Lens.# pKey_,
        keyArn = pKeyArn_,
        keyName = pKeyName_,
        restrictions = pRestrictions_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | The optional description for the API key resource.
describeKeyResponse_description :: Lens.Lens' DescribeKeyResponse (Prelude.Maybe Prelude.Text)
describeKeyResponse_description = Lens.lens (\DescribeKeyResponse' {description} -> description) (\s@DescribeKeyResponse' {} a -> s {description = a} :: DescribeKeyResponse)

-- | Tags associated with the API key resource.
describeKeyResponse_tags :: Lens.Lens' DescribeKeyResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeKeyResponse_tags = Lens.lens (\DescribeKeyResponse' {tags} -> tags) (\s@DescribeKeyResponse' {} a -> s {tags = a} :: DescribeKeyResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeKeyResponse_httpStatus :: Lens.Lens' DescribeKeyResponse Prelude.Int
describeKeyResponse_httpStatus = Lens.lens (\DescribeKeyResponse' {httpStatus} -> httpStatus) (\s@DescribeKeyResponse' {} a -> s {httpStatus = a} :: DescribeKeyResponse)

-- | The timestamp for when the API key resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
describeKeyResponse_createTime :: Lens.Lens' DescribeKeyResponse Prelude.UTCTime
describeKeyResponse_createTime = Lens.lens (\DescribeKeyResponse' {createTime} -> createTime) (\s@DescribeKeyResponse' {} a -> s {createTime = a} :: DescribeKeyResponse) Prelude.. Data._Time

-- | The timestamp for when the API key resource will expire in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
describeKeyResponse_expireTime :: Lens.Lens' DescribeKeyResponse Prelude.UTCTime
describeKeyResponse_expireTime = Lens.lens (\DescribeKeyResponse' {expireTime} -> expireTime) (\s@DescribeKeyResponse' {} a -> s {expireTime = a} :: DescribeKeyResponse) Prelude.. Data._Time

-- | The key value\/string of an API key.
describeKeyResponse_key :: Lens.Lens' DescribeKeyResponse Prelude.Text
describeKeyResponse_key = Lens.lens (\DescribeKeyResponse' {key} -> key) (\s@DescribeKeyResponse' {} a -> s {key = a} :: DescribeKeyResponse) Prelude.. Data._Sensitive

-- | The Amazon Resource Name (ARN) for the API key resource. Used when you
-- need to specify a resource across all Amazon Web Services.
--
-- -   Format example: @arn:aws:geo:region:account-id:key\/ExampleKey@
describeKeyResponse_keyArn :: Lens.Lens' DescribeKeyResponse Prelude.Text
describeKeyResponse_keyArn = Lens.lens (\DescribeKeyResponse' {keyArn} -> keyArn) (\s@DescribeKeyResponse' {} a -> s {keyArn = a} :: DescribeKeyResponse)

-- | The name of the API key resource.
describeKeyResponse_keyName :: Lens.Lens' DescribeKeyResponse Prelude.Text
describeKeyResponse_keyName = Lens.lens (\DescribeKeyResponse' {keyName} -> keyName) (\s@DescribeKeyResponse' {} a -> s {keyName = a} :: DescribeKeyResponse)

-- | Undocumented member.
describeKeyResponse_restrictions :: Lens.Lens' DescribeKeyResponse ApiKeyRestrictions
describeKeyResponse_restrictions = Lens.lens (\DescribeKeyResponse' {restrictions} -> restrictions) (\s@DescribeKeyResponse' {} a -> s {restrictions = a} :: DescribeKeyResponse)

-- | The timestamp for when the API key resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
describeKeyResponse_updateTime :: Lens.Lens' DescribeKeyResponse Prelude.UTCTime
describeKeyResponse_updateTime = Lens.lens (\DescribeKeyResponse' {updateTime} -> updateTime) (\s@DescribeKeyResponse' {} a -> s {updateTime = a} :: DescribeKeyResponse) Prelude.. Data._Time

instance Prelude.NFData DescribeKeyResponse where
  rnf DescribeKeyResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf expireTime
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf keyArn
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf restrictions
      `Prelude.seq` Prelude.rnf updateTime
