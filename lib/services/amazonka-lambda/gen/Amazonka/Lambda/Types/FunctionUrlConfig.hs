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
-- Module      : Amazonka.Lambda.Types.FunctionUrlConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.FunctionUrlConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types.Cors
import Amazonka.Lambda.Types.FunctionUrlAuthType
import qualified Amazonka.Prelude as Prelude

-- | Details about a Lambda function URL.
--
-- /See:/ 'newFunctionUrlConfig' smart constructor.
data FunctionUrlConfig = FunctionUrlConfig'
  { -- | The
    -- <https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS cross-origin resource sharing (CORS)>
    -- settings for your function URL.
    cors :: Prelude.Maybe Cors,
    -- | The HTTP URL endpoint for your function.
    functionUrl :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of your function.
    functionArn :: Prelude.Text,
    -- | When the function URL was created, in
    -- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
    -- (YYYY-MM-DDThh:mm:ss.sTZD).
    creationTime :: Prelude.Text,
    -- | When the function URL configuration was last updated, in
    -- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
    -- (YYYY-MM-DDThh:mm:ss.sTZD).
    lastModifiedTime :: Prelude.Text,
    -- | The type of authentication that your function URL uses. Set to @AWS_IAM@
    -- if you want to restrict access to authenticated IAM users only. Set to
    -- @NONE@ if you want to bypass IAM authentication to create a public
    -- endpoint. For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/urls-auth.html Security and auth model for Lambda function URLs>.
    authType :: FunctionUrlAuthType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunctionUrlConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cors', 'functionUrlConfig_cors' - The
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS cross-origin resource sharing (CORS)>
-- settings for your function URL.
--
-- 'functionUrl', 'functionUrlConfig_functionUrl' - The HTTP URL endpoint for your function.
--
-- 'functionArn', 'functionUrlConfig_functionArn' - The Amazon Resource Name (ARN) of your function.
--
-- 'creationTime', 'functionUrlConfig_creationTime' - When the function URL was created, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- 'lastModifiedTime', 'functionUrlConfig_lastModifiedTime' - When the function URL configuration was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- 'authType', 'functionUrlConfig_authType' - The type of authentication that your function URL uses. Set to @AWS_IAM@
-- if you want to restrict access to authenticated IAM users only. Set to
-- @NONE@ if you want to bypass IAM authentication to create a public
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/urls-auth.html Security and auth model for Lambda function URLs>.
newFunctionUrlConfig ::
  -- | 'functionUrl'
  Prelude.Text ->
  -- | 'functionArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.Text ->
  -- | 'lastModifiedTime'
  Prelude.Text ->
  -- | 'authType'
  FunctionUrlAuthType ->
  FunctionUrlConfig
newFunctionUrlConfig
  pFunctionUrl_
  pFunctionArn_
  pCreationTime_
  pLastModifiedTime_
  pAuthType_ =
    FunctionUrlConfig'
      { cors = Prelude.Nothing,
        functionUrl = pFunctionUrl_,
        functionArn = pFunctionArn_,
        creationTime = pCreationTime_,
        lastModifiedTime = pLastModifiedTime_,
        authType = pAuthType_
      }

-- | The
-- <https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS cross-origin resource sharing (CORS)>
-- settings for your function URL.
functionUrlConfig_cors :: Lens.Lens' FunctionUrlConfig (Prelude.Maybe Cors)
functionUrlConfig_cors = Lens.lens (\FunctionUrlConfig' {cors} -> cors) (\s@FunctionUrlConfig' {} a -> s {cors = a} :: FunctionUrlConfig)

-- | The HTTP URL endpoint for your function.
functionUrlConfig_functionUrl :: Lens.Lens' FunctionUrlConfig Prelude.Text
functionUrlConfig_functionUrl = Lens.lens (\FunctionUrlConfig' {functionUrl} -> functionUrl) (\s@FunctionUrlConfig' {} a -> s {functionUrl = a} :: FunctionUrlConfig)

-- | The Amazon Resource Name (ARN) of your function.
functionUrlConfig_functionArn :: Lens.Lens' FunctionUrlConfig Prelude.Text
functionUrlConfig_functionArn = Lens.lens (\FunctionUrlConfig' {functionArn} -> functionArn) (\s@FunctionUrlConfig' {} a -> s {functionArn = a} :: FunctionUrlConfig)

-- | When the function URL was created, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
functionUrlConfig_creationTime :: Lens.Lens' FunctionUrlConfig Prelude.Text
functionUrlConfig_creationTime = Lens.lens (\FunctionUrlConfig' {creationTime} -> creationTime) (\s@FunctionUrlConfig' {} a -> s {creationTime = a} :: FunctionUrlConfig)

-- | When the function URL configuration was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
functionUrlConfig_lastModifiedTime :: Lens.Lens' FunctionUrlConfig Prelude.Text
functionUrlConfig_lastModifiedTime = Lens.lens (\FunctionUrlConfig' {lastModifiedTime} -> lastModifiedTime) (\s@FunctionUrlConfig' {} a -> s {lastModifiedTime = a} :: FunctionUrlConfig)

-- | The type of authentication that your function URL uses. Set to @AWS_IAM@
-- if you want to restrict access to authenticated IAM users only. Set to
-- @NONE@ if you want to bypass IAM authentication to create a public
-- endpoint. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/urls-auth.html Security and auth model for Lambda function URLs>.
functionUrlConfig_authType :: Lens.Lens' FunctionUrlConfig FunctionUrlAuthType
functionUrlConfig_authType = Lens.lens (\FunctionUrlConfig' {authType} -> authType) (\s@FunctionUrlConfig' {} a -> s {authType = a} :: FunctionUrlConfig)

instance Data.FromJSON FunctionUrlConfig where
  parseJSON =
    Data.withObject
      "FunctionUrlConfig"
      ( \x ->
          FunctionUrlConfig'
            Prelude.<$> (x Data..:? "Cors")
            Prelude.<*> (x Data..: "FunctionUrl")
            Prelude.<*> (x Data..: "FunctionArn")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "LastModifiedTime")
            Prelude.<*> (x Data..: "AuthType")
      )

instance Prelude.Hashable FunctionUrlConfig where
  hashWithSalt _salt FunctionUrlConfig' {..} =
    _salt `Prelude.hashWithSalt` cors
      `Prelude.hashWithSalt` functionUrl
      `Prelude.hashWithSalt` functionArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` authType

instance Prelude.NFData FunctionUrlConfig where
  rnf FunctionUrlConfig' {..} =
    Prelude.rnf cors
      `Prelude.seq` Prelude.rnf functionUrl
      `Prelude.seq` Prelude.rnf functionArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf authType
