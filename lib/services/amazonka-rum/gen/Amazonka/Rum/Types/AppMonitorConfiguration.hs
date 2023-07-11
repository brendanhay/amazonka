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
-- Module      : Amazonka.Rum.Types.AppMonitorConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rum.Types.AppMonitorConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rum.Types.Telemetry

-- | This structure contains much of the configuration data for the app
-- monitor.
--
-- /See:/ 'newAppMonitorConfiguration' smart constructor.
data AppMonitorConfiguration = AppMonitorConfiguration'
  { -- | If you set this to @true@, the RUM web client sets two cookies, a
    -- session cookie and a user cookie. The cookies allow the RUM web client
    -- to collect data relating to the number of users an application has and
    -- the behavior of the application across a sequence of events. Cookies are
    -- stored in the top-level domain of the current page.
    allowCookies :: Prelude.Maybe Prelude.Bool,
    -- | If you set this to @true@, RUM enables X-Ray tracing for the user
    -- sessions that RUM samples. RUM adds an X-Ray trace header to allowed
    -- HTTP requests. It also records an X-Ray segment for allowed HTTP
    -- requests. You can see traces and segments from these user sessions in
    -- the X-Ray console and the CloudWatch ServiceLens console. For more
    -- information, see
    -- <https://docs.aws.amazon.com/xray/latest/devguide/aws-xray.html What is X-Ray?>
    enableXRay :: Prelude.Maybe Prelude.Bool,
    -- | A list of URLs in your website or application to exclude from RUM data
    -- collection.
    --
    -- You can\'t include both @ExcludedPages@ and @IncludedPages@ in the same
    -- operation.
    excludedPages :: Prelude.Maybe [Prelude.Text],
    -- | A list of pages in your application that are to be displayed with a
    -- \"favorite\" icon in the CloudWatch RUM console.
    favoritePages :: Prelude.Maybe [Prelude.Text],
    -- | The ARN of the guest IAM role that is attached to the Amazon Cognito
    -- identity pool that is used to authorize the sending of data to RUM.
    guestRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Cognito identity pool that is used to authorize the
    -- sending of data to RUM.
    identityPoolId :: Prelude.Maybe Prelude.Text,
    -- | If this app monitor is to collect data from only certain pages in your
    -- application, this structure lists those pages.
    --
    -- You can\'t include both @ExcludedPages@ and @IncludedPages@ in the same
    -- operation.
    includedPages :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the portion of user sessions to use for RUM data collection.
    -- Choosing a higher portion gives you more data but also incurs more
    -- costs.
    --
    -- The range for this value is 0 to 1 inclusive. Setting this to 1 means
    -- that 100% of user sessions are sampled, and setting it to 0.1 means that
    -- 10% of user sessions are sampled.
    --
    -- If you omit this parameter, the default of 0.1 is used, and 10% of
    -- sessions will be sampled.
    sessionSampleRate :: Prelude.Maybe Prelude.Double,
    -- | An array that lists the types of telemetry data that this app monitor is
    -- to collect.
    --
    -- -   @errors@ indicates that RUM collects data about unhandled JavaScript
    --     errors raised by your application.
    --
    -- -   @performance@ indicates that RUM collects performance data about how
    --     your application and its resources are loaded and rendered. This
    --     includes Core Web Vitals.
    --
    -- -   @http@ indicates that RUM collects data about HTTP errors thrown by
    --     your application.
    telemetries :: Prelude.Maybe [Telemetry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AppMonitorConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowCookies', 'appMonitorConfiguration_allowCookies' - If you set this to @true@, the RUM web client sets two cookies, a
-- session cookie and a user cookie. The cookies allow the RUM web client
-- to collect data relating to the number of users an application has and
-- the behavior of the application across a sequence of events. Cookies are
-- stored in the top-level domain of the current page.
--
-- 'enableXRay', 'appMonitorConfiguration_enableXRay' - If you set this to @true@, RUM enables X-Ray tracing for the user
-- sessions that RUM samples. RUM adds an X-Ray trace header to allowed
-- HTTP requests. It also records an X-Ray segment for allowed HTTP
-- requests. You can see traces and segments from these user sessions in
-- the X-Ray console and the CloudWatch ServiceLens console. For more
-- information, see
-- <https://docs.aws.amazon.com/xray/latest/devguide/aws-xray.html What is X-Ray?>
--
-- 'excludedPages', 'appMonitorConfiguration_excludedPages' - A list of URLs in your website or application to exclude from RUM data
-- collection.
--
-- You can\'t include both @ExcludedPages@ and @IncludedPages@ in the same
-- operation.
--
-- 'favoritePages', 'appMonitorConfiguration_favoritePages' - A list of pages in your application that are to be displayed with a
-- \"favorite\" icon in the CloudWatch RUM console.
--
-- 'guestRoleArn', 'appMonitorConfiguration_guestRoleArn' - The ARN of the guest IAM role that is attached to the Amazon Cognito
-- identity pool that is used to authorize the sending of data to RUM.
--
-- 'identityPoolId', 'appMonitorConfiguration_identityPoolId' - The ID of the Amazon Cognito identity pool that is used to authorize the
-- sending of data to RUM.
--
-- 'includedPages', 'appMonitorConfiguration_includedPages' - If this app monitor is to collect data from only certain pages in your
-- application, this structure lists those pages.
--
-- You can\'t include both @ExcludedPages@ and @IncludedPages@ in the same
-- operation.
--
-- 'sessionSampleRate', 'appMonitorConfiguration_sessionSampleRate' - Specifies the portion of user sessions to use for RUM data collection.
-- Choosing a higher portion gives you more data but also incurs more
-- costs.
--
-- The range for this value is 0 to 1 inclusive. Setting this to 1 means
-- that 100% of user sessions are sampled, and setting it to 0.1 means that
-- 10% of user sessions are sampled.
--
-- If you omit this parameter, the default of 0.1 is used, and 10% of
-- sessions will be sampled.
--
-- 'telemetries', 'appMonitorConfiguration_telemetries' - An array that lists the types of telemetry data that this app monitor is
-- to collect.
--
-- -   @errors@ indicates that RUM collects data about unhandled JavaScript
--     errors raised by your application.
--
-- -   @performance@ indicates that RUM collects performance data about how
--     your application and its resources are loaded and rendered. This
--     includes Core Web Vitals.
--
-- -   @http@ indicates that RUM collects data about HTTP errors thrown by
--     your application.
newAppMonitorConfiguration ::
  AppMonitorConfiguration
newAppMonitorConfiguration =
  AppMonitorConfiguration'
    { allowCookies =
        Prelude.Nothing,
      enableXRay = Prelude.Nothing,
      excludedPages = Prelude.Nothing,
      favoritePages = Prelude.Nothing,
      guestRoleArn = Prelude.Nothing,
      identityPoolId = Prelude.Nothing,
      includedPages = Prelude.Nothing,
      sessionSampleRate = Prelude.Nothing,
      telemetries = Prelude.Nothing
    }

-- | If you set this to @true@, the RUM web client sets two cookies, a
-- session cookie and a user cookie. The cookies allow the RUM web client
-- to collect data relating to the number of users an application has and
-- the behavior of the application across a sequence of events. Cookies are
-- stored in the top-level domain of the current page.
appMonitorConfiguration_allowCookies :: Lens.Lens' AppMonitorConfiguration (Prelude.Maybe Prelude.Bool)
appMonitorConfiguration_allowCookies = Lens.lens (\AppMonitorConfiguration' {allowCookies} -> allowCookies) (\s@AppMonitorConfiguration' {} a -> s {allowCookies = a} :: AppMonitorConfiguration)

-- | If you set this to @true@, RUM enables X-Ray tracing for the user
-- sessions that RUM samples. RUM adds an X-Ray trace header to allowed
-- HTTP requests. It also records an X-Ray segment for allowed HTTP
-- requests. You can see traces and segments from these user sessions in
-- the X-Ray console and the CloudWatch ServiceLens console. For more
-- information, see
-- <https://docs.aws.amazon.com/xray/latest/devguide/aws-xray.html What is X-Ray?>
appMonitorConfiguration_enableXRay :: Lens.Lens' AppMonitorConfiguration (Prelude.Maybe Prelude.Bool)
appMonitorConfiguration_enableXRay = Lens.lens (\AppMonitorConfiguration' {enableXRay} -> enableXRay) (\s@AppMonitorConfiguration' {} a -> s {enableXRay = a} :: AppMonitorConfiguration)

-- | A list of URLs in your website or application to exclude from RUM data
-- collection.
--
-- You can\'t include both @ExcludedPages@ and @IncludedPages@ in the same
-- operation.
appMonitorConfiguration_excludedPages :: Lens.Lens' AppMonitorConfiguration (Prelude.Maybe [Prelude.Text])
appMonitorConfiguration_excludedPages = Lens.lens (\AppMonitorConfiguration' {excludedPages} -> excludedPages) (\s@AppMonitorConfiguration' {} a -> s {excludedPages = a} :: AppMonitorConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of pages in your application that are to be displayed with a
-- \"favorite\" icon in the CloudWatch RUM console.
appMonitorConfiguration_favoritePages :: Lens.Lens' AppMonitorConfiguration (Prelude.Maybe [Prelude.Text])
appMonitorConfiguration_favoritePages = Lens.lens (\AppMonitorConfiguration' {favoritePages} -> favoritePages) (\s@AppMonitorConfiguration' {} a -> s {favoritePages = a} :: AppMonitorConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the guest IAM role that is attached to the Amazon Cognito
-- identity pool that is used to authorize the sending of data to RUM.
appMonitorConfiguration_guestRoleArn :: Lens.Lens' AppMonitorConfiguration (Prelude.Maybe Prelude.Text)
appMonitorConfiguration_guestRoleArn = Lens.lens (\AppMonitorConfiguration' {guestRoleArn} -> guestRoleArn) (\s@AppMonitorConfiguration' {} a -> s {guestRoleArn = a} :: AppMonitorConfiguration)

-- | The ID of the Amazon Cognito identity pool that is used to authorize the
-- sending of data to RUM.
appMonitorConfiguration_identityPoolId :: Lens.Lens' AppMonitorConfiguration (Prelude.Maybe Prelude.Text)
appMonitorConfiguration_identityPoolId = Lens.lens (\AppMonitorConfiguration' {identityPoolId} -> identityPoolId) (\s@AppMonitorConfiguration' {} a -> s {identityPoolId = a} :: AppMonitorConfiguration)

-- | If this app monitor is to collect data from only certain pages in your
-- application, this structure lists those pages.
--
-- You can\'t include both @ExcludedPages@ and @IncludedPages@ in the same
-- operation.
appMonitorConfiguration_includedPages :: Lens.Lens' AppMonitorConfiguration (Prelude.Maybe [Prelude.Text])
appMonitorConfiguration_includedPages = Lens.lens (\AppMonitorConfiguration' {includedPages} -> includedPages) (\s@AppMonitorConfiguration' {} a -> s {includedPages = a} :: AppMonitorConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the portion of user sessions to use for RUM data collection.
-- Choosing a higher portion gives you more data but also incurs more
-- costs.
--
-- The range for this value is 0 to 1 inclusive. Setting this to 1 means
-- that 100% of user sessions are sampled, and setting it to 0.1 means that
-- 10% of user sessions are sampled.
--
-- If you omit this parameter, the default of 0.1 is used, and 10% of
-- sessions will be sampled.
appMonitorConfiguration_sessionSampleRate :: Lens.Lens' AppMonitorConfiguration (Prelude.Maybe Prelude.Double)
appMonitorConfiguration_sessionSampleRate = Lens.lens (\AppMonitorConfiguration' {sessionSampleRate} -> sessionSampleRate) (\s@AppMonitorConfiguration' {} a -> s {sessionSampleRate = a} :: AppMonitorConfiguration)

-- | An array that lists the types of telemetry data that this app monitor is
-- to collect.
--
-- -   @errors@ indicates that RUM collects data about unhandled JavaScript
--     errors raised by your application.
--
-- -   @performance@ indicates that RUM collects performance data about how
--     your application and its resources are loaded and rendered. This
--     includes Core Web Vitals.
--
-- -   @http@ indicates that RUM collects data about HTTP errors thrown by
--     your application.
appMonitorConfiguration_telemetries :: Lens.Lens' AppMonitorConfiguration (Prelude.Maybe [Telemetry])
appMonitorConfiguration_telemetries = Lens.lens (\AppMonitorConfiguration' {telemetries} -> telemetries) (\s@AppMonitorConfiguration' {} a -> s {telemetries = a} :: AppMonitorConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AppMonitorConfiguration where
  parseJSON =
    Data.withObject
      "AppMonitorConfiguration"
      ( \x ->
          AppMonitorConfiguration'
            Prelude.<$> (x Data..:? "AllowCookies")
            Prelude.<*> (x Data..:? "EnableXRay")
            Prelude.<*> (x Data..:? "ExcludedPages" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "FavoritePages" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "GuestRoleArn")
            Prelude.<*> (x Data..:? "IdentityPoolId")
            Prelude.<*> (x Data..:? "IncludedPages" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SessionSampleRate")
            Prelude.<*> (x Data..:? "Telemetries" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AppMonitorConfiguration where
  hashWithSalt _salt AppMonitorConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` allowCookies
      `Prelude.hashWithSalt` enableXRay
      `Prelude.hashWithSalt` excludedPages
      `Prelude.hashWithSalt` favoritePages
      `Prelude.hashWithSalt` guestRoleArn
      `Prelude.hashWithSalt` identityPoolId
      `Prelude.hashWithSalt` includedPages
      `Prelude.hashWithSalt` sessionSampleRate
      `Prelude.hashWithSalt` telemetries

instance Prelude.NFData AppMonitorConfiguration where
  rnf AppMonitorConfiguration' {..} =
    Prelude.rnf allowCookies
      `Prelude.seq` Prelude.rnf enableXRay
      `Prelude.seq` Prelude.rnf excludedPages
      `Prelude.seq` Prelude.rnf favoritePages
      `Prelude.seq` Prelude.rnf guestRoleArn
      `Prelude.seq` Prelude.rnf identityPoolId
      `Prelude.seq` Prelude.rnf includedPages
      `Prelude.seq` Prelude.rnf sessionSampleRate
      `Prelude.seq` Prelude.rnf telemetries

instance Data.ToJSON AppMonitorConfiguration where
  toJSON AppMonitorConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AllowCookies" Data..=) Prelude.<$> allowCookies,
            ("EnableXRay" Data..=) Prelude.<$> enableXRay,
            ("ExcludedPages" Data..=) Prelude.<$> excludedPages,
            ("FavoritePages" Data..=) Prelude.<$> favoritePages,
            ("GuestRoleArn" Data..=) Prelude.<$> guestRoleArn,
            ("IdentityPoolId" Data..=)
              Prelude.<$> identityPoolId,
            ("IncludedPages" Data..=) Prelude.<$> includedPages,
            ("SessionSampleRate" Data..=)
              Prelude.<$> sessionSampleRate,
            ("Telemetries" Data..=) Prelude.<$> telemetries
          ]
      )
