{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.CreateAPIKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create an 'ApiKey' resource.
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/create-api-key.html AWS CLI>
module Network.AWS.APIGateway.CreateAPIKey
  ( -- * Creating a request
    CreateAPIKey (..),
    mkCreateAPIKey,

    -- ** Request lenses
    cakEnabled,
    cakValue,
    cakCustomerId,
    cakGenerateDistinctId,
    cakName,
    cakStageKeys,
    cakDescription,
    cakTags,

    -- * Destructuring the response
    APIKey (..),
    mkAPIKey,

    -- ** Response lenses
    akEnabled,
    akValue,
    akCustomerId,
    akCreatedDate,
    akName,
    akId,
    akStageKeys,
    akLastUpdatedDate,
    akDescription,
    akTags,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to create an 'ApiKey' resource.
--
-- /See:/ 'mkCreateAPIKey' smart constructor.
data CreateAPIKey = CreateAPIKey'
  { enabled :: Lude.Maybe Lude.Bool,
    value :: Lude.Maybe Lude.Text,
    customerId :: Lude.Maybe Lude.Text,
    generateDistinctId :: Lude.Maybe Lude.Bool,
    name :: Lude.Maybe Lude.Text,
    stageKeys :: Lude.Maybe [StageKey],
    description :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAPIKey' with the minimum fields required to make a request.
--
-- * 'customerId' - An AWS Marketplace customer identifier , when integrating with the AWS SaaS Marketplace.
-- * 'description' - The description of the 'ApiKey' .
-- * 'enabled' - Specifies whether the 'ApiKey' can be used by callers.
-- * 'generateDistinctId' - Specifies whether (@true@ ) or not (@false@ ) the key identifier is distinct from the created API key value. This parameter is deprecated and should not be used.
-- * 'name' - The name of the 'ApiKey' .
-- * 'stageKeys' - DEPRECATED FOR USAGE PLANS - Specifies stages associated with the API key.
-- * 'tags' - The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
-- * 'value' - Specifies a value of the API key.
mkCreateAPIKey ::
  CreateAPIKey
mkCreateAPIKey =
  CreateAPIKey'
    { enabled = Lude.Nothing,
      value = Lude.Nothing,
      customerId = Lude.Nothing,
      generateDistinctId = Lude.Nothing,
      name = Lude.Nothing,
      stageKeys = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | Specifies whether the 'ApiKey' can be used by callers.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakEnabled :: Lens.Lens' CreateAPIKey (Lude.Maybe Lude.Bool)
cakEnabled = Lens.lens (enabled :: CreateAPIKey -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: CreateAPIKey)
{-# DEPRECATED cakEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Specifies a value of the API key.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakValue :: Lens.Lens' CreateAPIKey (Lude.Maybe Lude.Text)
cakValue = Lens.lens (value :: CreateAPIKey -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: CreateAPIKey)
{-# DEPRECATED cakValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | An AWS Marketplace customer identifier , when integrating with the AWS SaaS Marketplace.
--
-- /Note:/ Consider using 'customerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakCustomerId :: Lens.Lens' CreateAPIKey (Lude.Maybe Lude.Text)
cakCustomerId = Lens.lens (customerId :: CreateAPIKey -> Lude.Maybe Lude.Text) (\s a -> s {customerId = a} :: CreateAPIKey)
{-# DEPRECATED cakCustomerId "Use generic-lens or generic-optics with 'customerId' instead." #-}

-- | Specifies whether (@true@ ) or not (@false@ ) the key identifier is distinct from the created API key value. This parameter is deprecated and should not be used.
--
-- /Note:/ Consider using 'generateDistinctId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakGenerateDistinctId :: Lens.Lens' CreateAPIKey (Lude.Maybe Lude.Bool)
cakGenerateDistinctId = Lens.lens (generateDistinctId :: CreateAPIKey -> Lude.Maybe Lude.Bool) (\s a -> s {generateDistinctId = a} :: CreateAPIKey)
{-# DEPRECATED cakGenerateDistinctId "Use generic-lens or generic-optics with 'generateDistinctId' instead." #-}

-- | The name of the 'ApiKey' .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakName :: Lens.Lens' CreateAPIKey (Lude.Maybe Lude.Text)
cakName = Lens.lens (name :: CreateAPIKey -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateAPIKey)
{-# DEPRECATED cakName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | DEPRECATED FOR USAGE PLANS - Specifies stages associated with the API key.
--
-- /Note:/ Consider using 'stageKeys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakStageKeys :: Lens.Lens' CreateAPIKey (Lude.Maybe [StageKey])
cakStageKeys = Lens.lens (stageKeys :: CreateAPIKey -> Lude.Maybe [StageKey]) (\s a -> s {stageKeys = a} :: CreateAPIKey)
{-# DEPRECATED cakStageKeys "Use generic-lens or generic-optics with 'stageKeys' instead." #-}

-- | The description of the 'ApiKey' .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakDescription :: Lens.Lens' CreateAPIKey (Lude.Maybe Lude.Text)
cakDescription = Lens.lens (description :: CreateAPIKey -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateAPIKey)
{-# DEPRECATED cakDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The key-value map of strings. The valid character set is [a-zA-Z+-=._:/]. The tag key can be up to 128 characters and must not start with @aws:@ . The tag value can be up to 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cakTags :: Lens.Lens' CreateAPIKey (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cakTags = Lens.lens (tags :: CreateAPIKey -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateAPIKey)
{-# DEPRECATED cakTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateAPIKey where
  type Rs CreateAPIKey = APIKey
  request = Req.postJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateAPIKey where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON CreateAPIKey where
  toJSON CreateAPIKey' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("enabled" Lude..=) Lude.<$> enabled,
            ("value" Lude..=) Lude.<$> value,
            ("customerId" Lude..=) Lude.<$> customerId,
            ("generateDistinctId" Lude..=) Lude.<$> generateDistinctId,
            ("name" Lude..=) Lude.<$> name,
            ("stageKeys" Lude..=) Lude.<$> stageKeys,
            ("description" Lude..=) Lude.<$> description,
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateAPIKey where
  toPath = Lude.const "/apikeys"

instance Lude.ToQuery CreateAPIKey where
  toQuery = Lude.const Lude.mempty
