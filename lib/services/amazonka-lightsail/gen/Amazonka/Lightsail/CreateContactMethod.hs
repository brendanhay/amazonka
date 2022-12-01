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
-- Module      : Amazonka.Lightsail.CreateContactMethod
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an email or SMS text message contact method.
--
-- A contact method is used to send you notifications about your Amazon
-- Lightsail resources. You can add one email address and one mobile phone
-- number contact method in each Amazon Web Services Region. However, SMS
-- text messaging is not supported in some Amazon Web Services Regions, and
-- SMS text messages cannot be sent to some countries\/regions. For more
-- information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail>.
module Amazonka.Lightsail.CreateContactMethod
  ( -- * Creating a Request
    CreateContactMethod (..),
    newCreateContactMethod,

    -- * Request Lenses
    createContactMethod_protocol,
    createContactMethod_contactEndpoint,

    -- * Destructuring the Response
    CreateContactMethodResponse (..),
    newCreateContactMethodResponse,

    -- * Response Lenses
    createContactMethodResponse_operations,
    createContactMethodResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateContactMethod' smart constructor.
data CreateContactMethod = CreateContactMethod'
  { -- | The protocol of the contact method, such as @Email@ or @SMS@ (text
    -- messaging).
    --
    -- The @SMS@ protocol is supported only in the following Amazon Web
    -- Services Regions.
    --
    -- -   US East (N. Virginia) (@us-east-1@)
    --
    -- -   US West (Oregon) (@us-west-2@)
    --
    -- -   Europe (Ireland) (@eu-west-1@)
    --
    -- -   Asia Pacific (Tokyo) (@ap-northeast-1@)
    --
    -- -   Asia Pacific (Singapore) (@ap-southeast-1@)
    --
    -- -   Asia Pacific (Sydney) (@ap-southeast-2@)
    --
    -- For a list of countries\/regions where SMS text messages can be sent,
    -- and the latest Amazon Web Services Regions where SMS text messaging is
    -- supported, see
    -- <https://docs.aws.amazon.com/sns/latest/dg/sns-supported-regions-countries.html Supported Regions and Countries>
    -- in the /Amazon SNS Developer Guide/.
    --
    -- For more information about notifications in Amazon Lightsail, see
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail>.
    protocol :: ContactProtocol,
    -- | The destination of the contact method, such as an email address or a
    -- mobile phone number.
    --
    -- Use the E.164 format when specifying a mobile phone number. E.164 is a
    -- standard for the phone number structure used for international
    -- telecommunication. Phone numbers that follow this format can have a
    -- maximum of 15 digits, and they are prefixed with the plus character (+)
    -- and the country code. For example, a U.S. phone number in E.164 format
    -- would be specified as +1XXX5550100. For more information, see
    -- <https://en.wikipedia.org/wiki/E.164 E.164> on /Wikipedia/.
    contactEndpoint :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContactMethod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'protocol', 'createContactMethod_protocol' - The protocol of the contact method, such as @Email@ or @SMS@ (text
-- messaging).
--
-- The @SMS@ protocol is supported only in the following Amazon Web
-- Services Regions.
--
-- -   US East (N. Virginia) (@us-east-1@)
--
-- -   US West (Oregon) (@us-west-2@)
--
-- -   Europe (Ireland) (@eu-west-1@)
--
-- -   Asia Pacific (Tokyo) (@ap-northeast-1@)
--
-- -   Asia Pacific (Singapore) (@ap-southeast-1@)
--
-- -   Asia Pacific (Sydney) (@ap-southeast-2@)
--
-- For a list of countries\/regions where SMS text messages can be sent,
-- and the latest Amazon Web Services Regions where SMS text messaging is
-- supported, see
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-supported-regions-countries.html Supported Regions and Countries>
-- in the /Amazon SNS Developer Guide/.
--
-- For more information about notifications in Amazon Lightsail, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail>.
--
-- 'contactEndpoint', 'createContactMethod_contactEndpoint' - The destination of the contact method, such as an email address or a
-- mobile phone number.
--
-- Use the E.164 format when specifying a mobile phone number. E.164 is a
-- standard for the phone number structure used for international
-- telecommunication. Phone numbers that follow this format can have a
-- maximum of 15 digits, and they are prefixed with the plus character (+)
-- and the country code. For example, a U.S. phone number in E.164 format
-- would be specified as +1XXX5550100. For more information, see
-- <https://en.wikipedia.org/wiki/E.164 E.164> on /Wikipedia/.
newCreateContactMethod ::
  -- | 'protocol'
  ContactProtocol ->
  -- | 'contactEndpoint'
  Prelude.Text ->
  CreateContactMethod
newCreateContactMethod pProtocol_ pContactEndpoint_ =
  CreateContactMethod'
    { protocol = pProtocol_,
      contactEndpoint = pContactEndpoint_
    }

-- | The protocol of the contact method, such as @Email@ or @SMS@ (text
-- messaging).
--
-- The @SMS@ protocol is supported only in the following Amazon Web
-- Services Regions.
--
-- -   US East (N. Virginia) (@us-east-1@)
--
-- -   US West (Oregon) (@us-west-2@)
--
-- -   Europe (Ireland) (@eu-west-1@)
--
-- -   Asia Pacific (Tokyo) (@ap-northeast-1@)
--
-- -   Asia Pacific (Singapore) (@ap-southeast-1@)
--
-- -   Asia Pacific (Sydney) (@ap-southeast-2@)
--
-- For a list of countries\/regions where SMS text messages can be sent,
-- and the latest Amazon Web Services Regions where SMS text messaging is
-- supported, see
-- <https://docs.aws.amazon.com/sns/latest/dg/sns-supported-regions-countries.html Supported Regions and Countries>
-- in the /Amazon SNS Developer Guide/.
--
-- For more information about notifications in Amazon Lightsail, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail>.
createContactMethod_protocol :: Lens.Lens' CreateContactMethod ContactProtocol
createContactMethod_protocol = Lens.lens (\CreateContactMethod' {protocol} -> protocol) (\s@CreateContactMethod' {} a -> s {protocol = a} :: CreateContactMethod)

-- | The destination of the contact method, such as an email address or a
-- mobile phone number.
--
-- Use the E.164 format when specifying a mobile phone number. E.164 is a
-- standard for the phone number structure used for international
-- telecommunication. Phone numbers that follow this format can have a
-- maximum of 15 digits, and they are prefixed with the plus character (+)
-- and the country code. For example, a U.S. phone number in E.164 format
-- would be specified as +1XXX5550100. For more information, see
-- <https://en.wikipedia.org/wiki/E.164 E.164> on /Wikipedia/.
createContactMethod_contactEndpoint :: Lens.Lens' CreateContactMethod Prelude.Text
createContactMethod_contactEndpoint = Lens.lens (\CreateContactMethod' {contactEndpoint} -> contactEndpoint) (\s@CreateContactMethod' {} a -> s {contactEndpoint = a} :: CreateContactMethod)

instance Core.AWSRequest CreateContactMethod where
  type
    AWSResponse CreateContactMethod =
      CreateContactMethodResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateContactMethodResponse'
            Prelude.<$> (x Core..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateContactMethod where
  hashWithSalt _salt CreateContactMethod' {..} =
    _salt `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` contactEndpoint

instance Prelude.NFData CreateContactMethod where
  rnf CreateContactMethod' {..} =
    Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf contactEndpoint

instance Core.ToHeaders CreateContactMethod where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateContactMethod" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateContactMethod where
  toJSON CreateContactMethod' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("protocol" Core..= protocol),
            Prelude.Just
              ("contactEndpoint" Core..= contactEndpoint)
          ]
      )

instance Core.ToPath CreateContactMethod where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateContactMethod where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateContactMethodResponse' smart constructor.
data CreateContactMethodResponse = CreateContactMethodResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateContactMethodResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'createContactMethodResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'createContactMethodResponse_httpStatus' - The response's http status code.
newCreateContactMethodResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateContactMethodResponse
newCreateContactMethodResponse pHttpStatus_ =
  CreateContactMethodResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createContactMethodResponse_operations :: Lens.Lens' CreateContactMethodResponse (Prelude.Maybe [Operation])
createContactMethodResponse_operations = Lens.lens (\CreateContactMethodResponse' {operations} -> operations) (\s@CreateContactMethodResponse' {} a -> s {operations = a} :: CreateContactMethodResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createContactMethodResponse_httpStatus :: Lens.Lens' CreateContactMethodResponse Prelude.Int
createContactMethodResponse_httpStatus = Lens.lens (\CreateContactMethodResponse' {httpStatus} -> httpStatus) (\s@CreateContactMethodResponse' {} a -> s {httpStatus = a} :: CreateContactMethodResponse)

instance Prelude.NFData CreateContactMethodResponse where
  rnf CreateContactMethodResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
