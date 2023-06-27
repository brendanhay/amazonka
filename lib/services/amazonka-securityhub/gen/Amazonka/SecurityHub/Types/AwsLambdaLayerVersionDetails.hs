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
-- Module      : Amazonka.SecurityHub.Types.AwsLambdaLayerVersionDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsLambdaLayerVersionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about a Lambda layer version.
--
-- /See:/ 'newAwsLambdaLayerVersionDetails' smart constructor.
data AwsLambdaLayerVersionDetails = AwsLambdaLayerVersionDetails'
  { -- | The layer\'s compatible runtimes. Maximum number of five items.
    --
    -- Valid values: @nodejs10.x@ | @nodejs12.x@ | @java8@ | @java11@ |
    -- @python2.7@ | @python3.6@ | @python3.7@ | @python3.8@ | @dotnetcore1.0@
    -- | @dotnetcore2.1@ | @go1.x@ | @ruby2.5@ | @provided@
    compatibleRuntimes :: Prelude.Maybe [Prelude.Text],
    -- | Indicates when the version was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces, and date and time should be separated
    -- by @T@. For example, @2020-03-22T13:22:13.933Z@.
    createdDate :: Prelude.Maybe Prelude.Text,
    -- | The version number.
    version :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsLambdaLayerVersionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'compatibleRuntimes', 'awsLambdaLayerVersionDetails_compatibleRuntimes' - The layer\'s compatible runtimes. Maximum number of five items.
--
-- Valid values: @nodejs10.x@ | @nodejs12.x@ | @java8@ | @java11@ |
-- @python2.7@ | @python3.6@ | @python3.7@ | @python3.8@ | @dotnetcore1.0@
-- | @dotnetcore2.1@ | @go1.x@ | @ruby2.5@ | @provided@
--
-- 'createdDate', 'awsLambdaLayerVersionDetails_createdDate' - Indicates when the version was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces, and date and time should be separated
-- by @T@. For example, @2020-03-22T13:22:13.933Z@.
--
-- 'version', 'awsLambdaLayerVersionDetails_version' - The version number.
newAwsLambdaLayerVersionDetails ::
  AwsLambdaLayerVersionDetails
newAwsLambdaLayerVersionDetails =
  AwsLambdaLayerVersionDetails'
    { compatibleRuntimes =
        Prelude.Nothing,
      createdDate = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The layer\'s compatible runtimes. Maximum number of five items.
--
-- Valid values: @nodejs10.x@ | @nodejs12.x@ | @java8@ | @java11@ |
-- @python2.7@ | @python3.6@ | @python3.7@ | @python3.8@ | @dotnetcore1.0@
-- | @dotnetcore2.1@ | @go1.x@ | @ruby2.5@ | @provided@
awsLambdaLayerVersionDetails_compatibleRuntimes :: Lens.Lens' AwsLambdaLayerVersionDetails (Prelude.Maybe [Prelude.Text])
awsLambdaLayerVersionDetails_compatibleRuntimes = Lens.lens (\AwsLambdaLayerVersionDetails' {compatibleRuntimes} -> compatibleRuntimes) (\s@AwsLambdaLayerVersionDetails' {} a -> s {compatibleRuntimes = a} :: AwsLambdaLayerVersionDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates when the version was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces, and date and time should be separated
-- by @T@. For example, @2020-03-22T13:22:13.933Z@.
awsLambdaLayerVersionDetails_createdDate :: Lens.Lens' AwsLambdaLayerVersionDetails (Prelude.Maybe Prelude.Text)
awsLambdaLayerVersionDetails_createdDate = Lens.lens (\AwsLambdaLayerVersionDetails' {createdDate} -> createdDate) (\s@AwsLambdaLayerVersionDetails' {} a -> s {createdDate = a} :: AwsLambdaLayerVersionDetails)

-- | The version number.
awsLambdaLayerVersionDetails_version :: Lens.Lens' AwsLambdaLayerVersionDetails (Prelude.Maybe Prelude.Integer)
awsLambdaLayerVersionDetails_version = Lens.lens (\AwsLambdaLayerVersionDetails' {version} -> version) (\s@AwsLambdaLayerVersionDetails' {} a -> s {version = a} :: AwsLambdaLayerVersionDetails)

instance Data.FromJSON AwsLambdaLayerVersionDetails where
  parseJSON =
    Data.withObject
      "AwsLambdaLayerVersionDetails"
      ( \x ->
          AwsLambdaLayerVersionDetails'
            Prelude.<$> ( x
                            Data..:? "CompatibleRuntimes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CreatedDate")
            Prelude.<*> (x Data..:? "Version")
      )

instance
  Prelude.Hashable
    AwsLambdaLayerVersionDetails
  where
  hashWithSalt _salt AwsLambdaLayerVersionDetails' {..} =
    _salt
      `Prelude.hashWithSalt` compatibleRuntimes
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` version

instance Prelude.NFData AwsLambdaLayerVersionDetails where
  rnf AwsLambdaLayerVersionDetails' {..} =
    Prelude.rnf compatibleRuntimes
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf version

instance Data.ToJSON AwsLambdaLayerVersionDetails where
  toJSON AwsLambdaLayerVersionDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CompatibleRuntimes" Data..=)
              Prelude.<$> compatibleRuntimes,
            ("CreatedDate" Data..=) Prelude.<$> createdDate,
            ("Version" Data..=) Prelude.<$> version
          ]
      )
