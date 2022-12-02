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
-- Module      : Amazonka.SESV2.GetConfigurationSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about an existing configuration set, including the
-- dedicated IP pool that it\'s associated with, whether or not it\'s
-- enabled for sending email, and more.
--
-- /Configuration sets/ are groups of rules that you can apply to the
-- emails you send. You apply a configuration set to an email by including
-- a reference to the configuration set in the headers of the email. When
-- you apply a configuration set to an email, all of the rules in that
-- configuration set are applied to the email.
module Amazonka.SESV2.GetConfigurationSet
  ( -- * Creating a Request
    GetConfigurationSet (..),
    newGetConfigurationSet,

    -- * Request Lenses
    getConfigurationSet_configurationSetName,

    -- * Destructuring the Response
    GetConfigurationSetResponse (..),
    newGetConfigurationSetResponse,

    -- * Response Lenses
    getConfigurationSetResponse_tags,
    getConfigurationSetResponse_reputationOptions,
    getConfigurationSetResponse_configurationSetName,
    getConfigurationSetResponse_deliveryOptions,
    getConfigurationSetResponse_trackingOptions,
    getConfigurationSetResponse_suppressionOptions,
    getConfigurationSetResponse_sendingOptions,
    getConfigurationSetResponse_vdmOptions,
    getConfigurationSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to obtain information about a configuration set.
--
-- /See:/ 'newGetConfigurationSet' smart constructor.
data GetConfigurationSet = GetConfigurationSet'
  { -- | The name of the configuration set.
    configurationSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConfigurationSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'getConfigurationSet_configurationSetName' - The name of the configuration set.
newGetConfigurationSet ::
  -- | 'configurationSetName'
  Prelude.Text ->
  GetConfigurationSet
newGetConfigurationSet pConfigurationSetName_ =
  GetConfigurationSet'
    { configurationSetName =
        pConfigurationSetName_
    }

-- | The name of the configuration set.
getConfigurationSet_configurationSetName :: Lens.Lens' GetConfigurationSet Prelude.Text
getConfigurationSet_configurationSetName = Lens.lens (\GetConfigurationSet' {configurationSetName} -> configurationSetName) (\s@GetConfigurationSet' {} a -> s {configurationSetName = a} :: GetConfigurationSet)

instance Core.AWSRequest GetConfigurationSet where
  type
    AWSResponse GetConfigurationSet =
      GetConfigurationSetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConfigurationSetResponse'
            Prelude.<$> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ReputationOptions")
            Prelude.<*> (x Data..?> "ConfigurationSetName")
            Prelude.<*> (x Data..?> "DeliveryOptions")
            Prelude.<*> (x Data..?> "TrackingOptions")
            Prelude.<*> (x Data..?> "SuppressionOptions")
            Prelude.<*> (x Data..?> "SendingOptions")
            Prelude.<*> (x Data..?> "VdmOptions")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConfigurationSet where
  hashWithSalt _salt GetConfigurationSet' {..} =
    _salt `Prelude.hashWithSalt` configurationSetName

instance Prelude.NFData GetConfigurationSet where
  rnf GetConfigurationSet' {..} =
    Prelude.rnf configurationSetName

instance Data.ToHeaders GetConfigurationSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetConfigurationSet where
  toPath GetConfigurationSet' {..} =
    Prelude.mconcat
      [ "/v2/email/configuration-sets/",
        Data.toBS configurationSetName
      ]

instance Data.ToQuery GetConfigurationSet where
  toQuery = Prelude.const Prelude.mempty

-- | Information about a configuration set.
--
-- /See:/ 'newGetConfigurationSetResponse' smart constructor.
data GetConfigurationSetResponse = GetConfigurationSetResponse'
  { -- | An array of objects that define the tags (keys and values) that are
    -- associated with the configuration set.
    tags :: Prelude.Maybe [Tag],
    -- | An object that defines whether or not Amazon SES collects reputation
    -- metrics for the emails that you send that use the configuration set.
    reputationOptions :: Prelude.Maybe ReputationOptions,
    -- | The name of the configuration set.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    -- | An object that defines the dedicated IP pool that is used to send emails
    -- that you send using the configuration set.
    deliveryOptions :: Prelude.Maybe DeliveryOptions,
    -- | An object that defines the open and click tracking options for emails
    -- that you send using the configuration set.
    trackingOptions :: Prelude.Maybe TrackingOptions,
    -- | An object that contains information about the suppression list
    -- preferences for your account.
    suppressionOptions :: Prelude.Maybe SuppressionOptions,
    -- | An object that defines whether or not Amazon SES can send email that you
    -- send using the configuration set.
    sendingOptions :: Prelude.Maybe SendingOptions,
    -- | An object that contains information about the VDM preferences for your
    -- configuration set.
    vdmOptions :: Prelude.Maybe VdmOptions,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConfigurationSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getConfigurationSetResponse_tags' - An array of objects that define the tags (keys and values) that are
-- associated with the configuration set.
--
-- 'reputationOptions', 'getConfigurationSetResponse_reputationOptions' - An object that defines whether or not Amazon SES collects reputation
-- metrics for the emails that you send that use the configuration set.
--
-- 'configurationSetName', 'getConfigurationSetResponse_configurationSetName' - The name of the configuration set.
--
-- 'deliveryOptions', 'getConfigurationSetResponse_deliveryOptions' - An object that defines the dedicated IP pool that is used to send emails
-- that you send using the configuration set.
--
-- 'trackingOptions', 'getConfigurationSetResponse_trackingOptions' - An object that defines the open and click tracking options for emails
-- that you send using the configuration set.
--
-- 'suppressionOptions', 'getConfigurationSetResponse_suppressionOptions' - An object that contains information about the suppression list
-- preferences for your account.
--
-- 'sendingOptions', 'getConfigurationSetResponse_sendingOptions' - An object that defines whether or not Amazon SES can send email that you
-- send using the configuration set.
--
-- 'vdmOptions', 'getConfigurationSetResponse_vdmOptions' - An object that contains information about the VDM preferences for your
-- configuration set.
--
-- 'httpStatus', 'getConfigurationSetResponse_httpStatus' - The response's http status code.
newGetConfigurationSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetConfigurationSetResponse
newGetConfigurationSetResponse pHttpStatus_ =
  GetConfigurationSetResponse'
    { tags =
        Prelude.Nothing,
      reputationOptions = Prelude.Nothing,
      configurationSetName = Prelude.Nothing,
      deliveryOptions = Prelude.Nothing,
      trackingOptions = Prelude.Nothing,
      suppressionOptions = Prelude.Nothing,
      sendingOptions = Prelude.Nothing,
      vdmOptions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that define the tags (keys and values) that are
-- associated with the configuration set.
getConfigurationSetResponse_tags :: Lens.Lens' GetConfigurationSetResponse (Prelude.Maybe [Tag])
getConfigurationSetResponse_tags = Lens.lens (\GetConfigurationSetResponse' {tags} -> tags) (\s@GetConfigurationSetResponse' {} a -> s {tags = a} :: GetConfigurationSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | An object that defines whether or not Amazon SES collects reputation
-- metrics for the emails that you send that use the configuration set.
getConfigurationSetResponse_reputationOptions :: Lens.Lens' GetConfigurationSetResponse (Prelude.Maybe ReputationOptions)
getConfigurationSetResponse_reputationOptions = Lens.lens (\GetConfigurationSetResponse' {reputationOptions} -> reputationOptions) (\s@GetConfigurationSetResponse' {} a -> s {reputationOptions = a} :: GetConfigurationSetResponse)

-- | The name of the configuration set.
getConfigurationSetResponse_configurationSetName :: Lens.Lens' GetConfigurationSetResponse (Prelude.Maybe Prelude.Text)
getConfigurationSetResponse_configurationSetName = Lens.lens (\GetConfigurationSetResponse' {configurationSetName} -> configurationSetName) (\s@GetConfigurationSetResponse' {} a -> s {configurationSetName = a} :: GetConfigurationSetResponse)

-- | An object that defines the dedicated IP pool that is used to send emails
-- that you send using the configuration set.
getConfigurationSetResponse_deliveryOptions :: Lens.Lens' GetConfigurationSetResponse (Prelude.Maybe DeliveryOptions)
getConfigurationSetResponse_deliveryOptions = Lens.lens (\GetConfigurationSetResponse' {deliveryOptions} -> deliveryOptions) (\s@GetConfigurationSetResponse' {} a -> s {deliveryOptions = a} :: GetConfigurationSetResponse)

-- | An object that defines the open and click tracking options for emails
-- that you send using the configuration set.
getConfigurationSetResponse_trackingOptions :: Lens.Lens' GetConfigurationSetResponse (Prelude.Maybe TrackingOptions)
getConfigurationSetResponse_trackingOptions = Lens.lens (\GetConfigurationSetResponse' {trackingOptions} -> trackingOptions) (\s@GetConfigurationSetResponse' {} a -> s {trackingOptions = a} :: GetConfigurationSetResponse)

-- | An object that contains information about the suppression list
-- preferences for your account.
getConfigurationSetResponse_suppressionOptions :: Lens.Lens' GetConfigurationSetResponse (Prelude.Maybe SuppressionOptions)
getConfigurationSetResponse_suppressionOptions = Lens.lens (\GetConfigurationSetResponse' {suppressionOptions} -> suppressionOptions) (\s@GetConfigurationSetResponse' {} a -> s {suppressionOptions = a} :: GetConfigurationSetResponse)

-- | An object that defines whether or not Amazon SES can send email that you
-- send using the configuration set.
getConfigurationSetResponse_sendingOptions :: Lens.Lens' GetConfigurationSetResponse (Prelude.Maybe SendingOptions)
getConfigurationSetResponse_sendingOptions = Lens.lens (\GetConfigurationSetResponse' {sendingOptions} -> sendingOptions) (\s@GetConfigurationSetResponse' {} a -> s {sendingOptions = a} :: GetConfigurationSetResponse)

-- | An object that contains information about the VDM preferences for your
-- configuration set.
getConfigurationSetResponse_vdmOptions :: Lens.Lens' GetConfigurationSetResponse (Prelude.Maybe VdmOptions)
getConfigurationSetResponse_vdmOptions = Lens.lens (\GetConfigurationSetResponse' {vdmOptions} -> vdmOptions) (\s@GetConfigurationSetResponse' {} a -> s {vdmOptions = a} :: GetConfigurationSetResponse)

-- | The response's http status code.
getConfigurationSetResponse_httpStatus :: Lens.Lens' GetConfigurationSetResponse Prelude.Int
getConfigurationSetResponse_httpStatus = Lens.lens (\GetConfigurationSetResponse' {httpStatus} -> httpStatus) (\s@GetConfigurationSetResponse' {} a -> s {httpStatus = a} :: GetConfigurationSetResponse)

instance Prelude.NFData GetConfigurationSetResponse where
  rnf GetConfigurationSetResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf reputationOptions
      `Prelude.seq` Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf deliveryOptions
      `Prelude.seq` Prelude.rnf trackingOptions
      `Prelude.seq` Prelude.rnf suppressionOptions
      `Prelude.seq` Prelude.rnf sendingOptions
      `Prelude.seq` Prelude.rnf vdmOptions
      `Prelude.seq` Prelude.rnf httpStatus
