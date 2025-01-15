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
-- Module      : Amazonka.PinpointEmail.CreateConfigurationSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a configuration set. /Configuration sets/ are groups of rules
-- that you can apply to the emails you send using Amazon Pinpoint. You
-- apply a configuration set to an email by including a reference to the
-- configuration set in the headers of the email. When you apply a
-- configuration set to an email, all of the rules in that configuration
-- set are applied to the email.
module Amazonka.PinpointEmail.CreateConfigurationSet
  ( -- * Creating a Request
    CreateConfigurationSet (..),
    newCreateConfigurationSet,

    -- * Request Lenses
    createConfigurationSet_deliveryOptions,
    createConfigurationSet_reputationOptions,
    createConfigurationSet_sendingOptions,
    createConfigurationSet_tags,
    createConfigurationSet_trackingOptions,
    createConfigurationSet_configurationSetName,

    -- * Destructuring the Response
    CreateConfigurationSetResponse (..),
    newCreateConfigurationSetResponse,

    -- * Response Lenses
    createConfigurationSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointEmail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to create a configuration set.
--
-- /See:/ 'newCreateConfigurationSet' smart constructor.
data CreateConfigurationSet = CreateConfigurationSet'
  { -- | An object that defines the dedicated IP pool that is used to send emails
    -- that you send using the configuration set.
    deliveryOptions :: Prelude.Maybe DeliveryOptions,
    -- | An object that defines whether or not Amazon Pinpoint collects
    -- reputation metrics for the emails that you send that use the
    -- configuration set.
    reputationOptions :: Prelude.Maybe ReputationOptions,
    -- | An object that defines whether or not Amazon Pinpoint can send email
    -- that you send using the configuration set.
    sendingOptions :: Prelude.Maybe SendingOptions,
    -- | An array of objects that define the tags (keys and values) that you want
    -- to associate with the configuration set.
    tags :: Prelude.Maybe [Tag],
    -- | An object that defines the open and click tracking options for emails
    -- that you send using the configuration set.
    trackingOptions :: Prelude.Maybe TrackingOptions,
    -- | The name of the configuration set.
    configurationSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConfigurationSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryOptions', 'createConfigurationSet_deliveryOptions' - An object that defines the dedicated IP pool that is used to send emails
-- that you send using the configuration set.
--
-- 'reputationOptions', 'createConfigurationSet_reputationOptions' - An object that defines whether or not Amazon Pinpoint collects
-- reputation metrics for the emails that you send that use the
-- configuration set.
--
-- 'sendingOptions', 'createConfigurationSet_sendingOptions' - An object that defines whether or not Amazon Pinpoint can send email
-- that you send using the configuration set.
--
-- 'tags', 'createConfigurationSet_tags' - An array of objects that define the tags (keys and values) that you want
-- to associate with the configuration set.
--
-- 'trackingOptions', 'createConfigurationSet_trackingOptions' - An object that defines the open and click tracking options for emails
-- that you send using the configuration set.
--
-- 'configurationSetName', 'createConfigurationSet_configurationSetName' - The name of the configuration set.
newCreateConfigurationSet ::
  -- | 'configurationSetName'
  Prelude.Text ->
  CreateConfigurationSet
newCreateConfigurationSet pConfigurationSetName_ =
  CreateConfigurationSet'
    { deliveryOptions =
        Prelude.Nothing,
      reputationOptions = Prelude.Nothing,
      sendingOptions = Prelude.Nothing,
      tags = Prelude.Nothing,
      trackingOptions = Prelude.Nothing,
      configurationSetName = pConfigurationSetName_
    }

-- | An object that defines the dedicated IP pool that is used to send emails
-- that you send using the configuration set.
createConfigurationSet_deliveryOptions :: Lens.Lens' CreateConfigurationSet (Prelude.Maybe DeliveryOptions)
createConfigurationSet_deliveryOptions = Lens.lens (\CreateConfigurationSet' {deliveryOptions} -> deliveryOptions) (\s@CreateConfigurationSet' {} a -> s {deliveryOptions = a} :: CreateConfigurationSet)

-- | An object that defines whether or not Amazon Pinpoint collects
-- reputation metrics for the emails that you send that use the
-- configuration set.
createConfigurationSet_reputationOptions :: Lens.Lens' CreateConfigurationSet (Prelude.Maybe ReputationOptions)
createConfigurationSet_reputationOptions = Lens.lens (\CreateConfigurationSet' {reputationOptions} -> reputationOptions) (\s@CreateConfigurationSet' {} a -> s {reputationOptions = a} :: CreateConfigurationSet)

-- | An object that defines whether or not Amazon Pinpoint can send email
-- that you send using the configuration set.
createConfigurationSet_sendingOptions :: Lens.Lens' CreateConfigurationSet (Prelude.Maybe SendingOptions)
createConfigurationSet_sendingOptions = Lens.lens (\CreateConfigurationSet' {sendingOptions} -> sendingOptions) (\s@CreateConfigurationSet' {} a -> s {sendingOptions = a} :: CreateConfigurationSet)

-- | An array of objects that define the tags (keys and values) that you want
-- to associate with the configuration set.
createConfigurationSet_tags :: Lens.Lens' CreateConfigurationSet (Prelude.Maybe [Tag])
createConfigurationSet_tags = Lens.lens (\CreateConfigurationSet' {tags} -> tags) (\s@CreateConfigurationSet' {} a -> s {tags = a} :: CreateConfigurationSet) Prelude.. Lens.mapping Lens.coerced

-- | An object that defines the open and click tracking options for emails
-- that you send using the configuration set.
createConfigurationSet_trackingOptions :: Lens.Lens' CreateConfigurationSet (Prelude.Maybe TrackingOptions)
createConfigurationSet_trackingOptions = Lens.lens (\CreateConfigurationSet' {trackingOptions} -> trackingOptions) (\s@CreateConfigurationSet' {} a -> s {trackingOptions = a} :: CreateConfigurationSet)

-- | The name of the configuration set.
createConfigurationSet_configurationSetName :: Lens.Lens' CreateConfigurationSet Prelude.Text
createConfigurationSet_configurationSetName = Lens.lens (\CreateConfigurationSet' {configurationSetName} -> configurationSetName) (\s@CreateConfigurationSet' {} a -> s {configurationSetName = a} :: CreateConfigurationSet)

instance Core.AWSRequest CreateConfigurationSet where
  type
    AWSResponse CreateConfigurationSet =
      CreateConfigurationSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateConfigurationSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateConfigurationSet where
  hashWithSalt _salt CreateConfigurationSet' {..} =
    _salt
      `Prelude.hashWithSalt` deliveryOptions
      `Prelude.hashWithSalt` reputationOptions
      `Prelude.hashWithSalt` sendingOptions
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` trackingOptions
      `Prelude.hashWithSalt` configurationSetName

instance Prelude.NFData CreateConfigurationSet where
  rnf CreateConfigurationSet' {..} =
    Prelude.rnf deliveryOptions `Prelude.seq`
      Prelude.rnf reputationOptions `Prelude.seq`
        Prelude.rnf sendingOptions `Prelude.seq`
          Prelude.rnf tags `Prelude.seq`
            Prelude.rnf trackingOptions `Prelude.seq`
              Prelude.rnf configurationSetName

instance Data.ToHeaders CreateConfigurationSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateConfigurationSet where
  toJSON CreateConfigurationSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeliveryOptions" Data..=)
              Prelude.<$> deliveryOptions,
            ("ReputationOptions" Data..=)
              Prelude.<$> reputationOptions,
            ("SendingOptions" Data..=)
              Prelude.<$> sendingOptions,
            ("Tags" Data..=) Prelude.<$> tags,
            ("TrackingOptions" Data..=)
              Prelude.<$> trackingOptions,
            Prelude.Just
              ( "ConfigurationSetName"
                  Data..= configurationSetName
              )
          ]
      )

instance Data.ToPath CreateConfigurationSet where
  toPath = Prelude.const "/v1/email/configuration-sets"

instance Data.ToQuery CreateConfigurationSet where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newCreateConfigurationSetResponse' smart constructor.
data CreateConfigurationSetResponse = CreateConfigurationSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConfigurationSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createConfigurationSetResponse_httpStatus' - The response's http status code.
newCreateConfigurationSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateConfigurationSetResponse
newCreateConfigurationSetResponse pHttpStatus_ =
  CreateConfigurationSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createConfigurationSetResponse_httpStatus :: Lens.Lens' CreateConfigurationSetResponse Prelude.Int
createConfigurationSetResponse_httpStatus = Lens.lens (\CreateConfigurationSetResponse' {httpStatus} -> httpStatus) (\s@CreateConfigurationSetResponse' {} a -> s {httpStatus = a} :: CreateConfigurationSetResponse)

instance
  Prelude.NFData
    CreateConfigurationSetResponse
  where
  rnf CreateConfigurationSetResponse' {..} =
    Prelude.rnf httpStatus
