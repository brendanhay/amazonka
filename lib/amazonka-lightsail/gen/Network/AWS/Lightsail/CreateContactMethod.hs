{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateContactMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an email or SMS text message contact method.
--
-- A contact method is used to send you notifications about your Amazon Lightsail resources. You can add one email address and one mobile phone number contact method in each AWS Region. However, SMS text messaging is not supported in some AWS Regions, and SMS text messages cannot be sent to some countries/regions. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail> .
module Network.AWS.Lightsail.CreateContactMethod
  ( -- * Creating a request
    CreateContactMethod (..),
    mkCreateContactMethod,

    -- ** Request lenses
    ccmProtocol,
    ccmContactEndpoint,

    -- * Destructuring the response
    CreateContactMethodResponse (..),
    mkCreateContactMethodResponse,

    -- ** Response lenses
    ccmrsOperations,
    ccmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateContactMethod' smart constructor.
data CreateContactMethod = CreateContactMethod'
  { -- | The protocol of the contact method, such as @Email@ or @SMS@ (text messaging).
    --
    -- The @SMS@ protocol is supported only in the following AWS Regions.
    --
    --     * US East (N. Virginia) (@us-east-1@ )
    --
    --
    --     * US West (Oregon) (@us-west-2@ )
    --
    --
    --     * Europe (Ireland) (@eu-west-1@ )
    --
    --
    --     * Asia Pacific (Tokyo) (@ap-northeast-1@ )
    --
    --
    --     * Asia Pacific (Singapore) (@ap-southeast-1@ )
    --
    --
    --     * Asia Pacific (Sydney) (@ap-southeast-2@ )
    --
    --
    -- For a list of countries/regions where SMS text messages can be sent, and the latest AWS Regions where SMS text messaging is supported, see <https://docs.aws.amazon.com/sns/latest/dg/sns-supported-regions-countries.html Supported Regions and Countries> in the /Amazon SNS Developer Guide/ .
    -- For more information about notifications in Amazon Lightsail, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail> .
    protocol :: ContactProtocol,
    -- | The destination of the contact method, such as an email address or a mobile phone number.
    --
    -- Use the E.164 format when specifying a mobile phone number. E.164 is a standard for the phone number structure used for international telecommunication. Phone numbers that follow this format can have a maximum of 15 digits, and they are prefixed with the plus character (+) and the country code. For example, a U.S. phone number in E.164 format would be specified as +1XXX5550100. For more information, see <https://en.wikipedia.org/wiki/E.164 E.164> on /Wikipedia/ .
    contactEndpoint :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateContactMethod' with the minimum fields required to make a request.
--
-- * 'protocol' - The protocol of the contact method, such as @Email@ or @SMS@ (text messaging).
--
-- The @SMS@ protocol is supported only in the following AWS Regions.
--
--     * US East (N. Virginia) (@us-east-1@ )
--
--
--     * US West (Oregon) (@us-west-2@ )
--
--
--     * Europe (Ireland) (@eu-west-1@ )
--
--
--     * Asia Pacific (Tokyo) (@ap-northeast-1@ )
--
--
--     * Asia Pacific (Singapore) (@ap-southeast-1@ )
--
--
--     * Asia Pacific (Sydney) (@ap-southeast-2@ )
--
--
-- For a list of countries/regions where SMS text messages can be sent, and the latest AWS Regions where SMS text messaging is supported, see <https://docs.aws.amazon.com/sns/latest/dg/sns-supported-regions-countries.html Supported Regions and Countries> in the /Amazon SNS Developer Guide/ .
-- For more information about notifications in Amazon Lightsail, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail> .
-- * 'contactEndpoint' - The destination of the contact method, such as an email address or a mobile phone number.
--
-- Use the E.164 format when specifying a mobile phone number. E.164 is a standard for the phone number structure used for international telecommunication. Phone numbers that follow this format can have a maximum of 15 digits, and they are prefixed with the plus character (+) and the country code. For example, a U.S. phone number in E.164 format would be specified as +1XXX5550100. For more information, see <https://en.wikipedia.org/wiki/E.164 E.164> on /Wikipedia/ .
mkCreateContactMethod ::
  -- | 'protocol'
  ContactProtocol ->
  -- | 'contactEndpoint'
  Lude.Text ->
  CreateContactMethod
mkCreateContactMethod pProtocol_ pContactEndpoint_ =
  CreateContactMethod'
    { protocol = pProtocol_,
      contactEndpoint = pContactEndpoint_
    }

-- | The protocol of the contact method, such as @Email@ or @SMS@ (text messaging).
--
-- The @SMS@ protocol is supported only in the following AWS Regions.
--
--     * US East (N. Virginia) (@us-east-1@ )
--
--
--     * US West (Oregon) (@us-west-2@ )
--
--
--     * Europe (Ireland) (@eu-west-1@ )
--
--
--     * Asia Pacific (Tokyo) (@ap-northeast-1@ )
--
--
--     * Asia Pacific (Singapore) (@ap-southeast-1@ )
--
--
--     * Asia Pacific (Sydney) (@ap-southeast-2@ )
--
--
-- For a list of countries/regions where SMS text messages can be sent, and the latest AWS Regions where SMS text messaging is supported, see <https://docs.aws.amazon.com/sns/latest/dg/sns-supported-regions-countries.html Supported Regions and Countries> in the /Amazon SNS Developer Guide/ .
-- For more information about notifications in Amazon Lightsail, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail> .
--
-- /Note:/ Consider using 'protocol' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmProtocol :: Lens.Lens' CreateContactMethod ContactProtocol
ccmProtocol = Lens.lens (protocol :: CreateContactMethod -> ContactProtocol) (\s a -> s {protocol = a} :: CreateContactMethod)
{-# DEPRECATED ccmProtocol "Use generic-lens or generic-optics with 'protocol' instead." #-}

-- | The destination of the contact method, such as an email address or a mobile phone number.
--
-- Use the E.164 format when specifying a mobile phone number. E.164 is a standard for the phone number structure used for international telecommunication. Phone numbers that follow this format can have a maximum of 15 digits, and they are prefixed with the plus character (+) and the country code. For example, a U.S. phone number in E.164 format would be specified as +1XXX5550100. For more information, see <https://en.wikipedia.org/wiki/E.164 E.164> on /Wikipedia/ .
--
-- /Note:/ Consider using 'contactEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmContactEndpoint :: Lens.Lens' CreateContactMethod Lude.Text
ccmContactEndpoint = Lens.lens (contactEndpoint :: CreateContactMethod -> Lude.Text) (\s a -> s {contactEndpoint = a} :: CreateContactMethod)
{-# DEPRECATED ccmContactEndpoint "Use generic-lens or generic-optics with 'contactEndpoint' instead." #-}

instance Lude.AWSRequest CreateContactMethod where
  type Rs CreateContactMethod = CreateContactMethodResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateContactMethodResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateContactMethod where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.CreateContactMethod" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateContactMethod where
  toJSON CreateContactMethod' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("protocol" Lude..= protocol),
            Lude.Just ("contactEndpoint" Lude..= contactEndpoint)
          ]
      )

instance Lude.ToPath CreateContactMethod where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateContactMethod where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateContactMethodResponse' smart constructor.
data CreateContactMethodResponse = CreateContactMethodResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateContactMethodResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkCreateContactMethodResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateContactMethodResponse
mkCreateContactMethodResponse pResponseStatus_ =
  CreateContactMethodResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmrsOperations :: Lens.Lens' CreateContactMethodResponse (Lude.Maybe [Operation])
ccmrsOperations = Lens.lens (operations :: CreateContactMethodResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: CreateContactMethodResponse)
{-# DEPRECATED ccmrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccmrsResponseStatus :: Lens.Lens' CreateContactMethodResponse Lude.Int
ccmrsResponseStatus = Lens.lens (responseStatus :: CreateContactMethodResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateContactMethodResponse)
{-# DEPRECATED ccmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
