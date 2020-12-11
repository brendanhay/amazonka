{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CreateInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more Amazon Lightsail instances.
--
-- The @create instances@ operation supports tag-based access control via request tags. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.CreateInstances
  ( -- * Creating a request
    CreateInstances (..),
    mkCreateInstances,

    -- ** Request lenses
    ciCustomImageName,
    ciAddOns,
    ciUserData,
    ciKeyPairName,
    ciTags,
    ciInstanceNames,
    ciAvailabilityZone,
    ciBlueprintId,
    ciBundleId,

    -- * Destructuring the response
    CreateInstancesResponse (..),
    mkCreateInstancesResponse,

    -- ** Response lenses
    cirsOperations,
    cirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateInstances' smart constructor.
data CreateInstances = CreateInstances'
  { customImageName ::
      Lude.Maybe Lude.Text,
    addOns :: Lude.Maybe [AddOnRequest],
    userData :: Lude.Maybe Lude.Text,
    keyPairName :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    instanceNames :: [Lude.Text],
    availabilityZone :: Lude.Text,
    blueprintId :: Lude.Text,
    bundleId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInstances' with the minimum fields required to make a request.
--
-- * 'addOns' - An array of objects representing the add-ons to enable for the new instance.
-- * 'availabilityZone' - The Availability Zone in which to create your instance. Use the following format: @us-east-2a@ (case sensitive). You can get a list of Availability Zones by using the <http://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetRegions.html get regions> operation. Be sure to add the @include Availability Zones@ parameter to your request.
-- * 'blueprintId' - The ID for a virtual private server image (e.g., @app_wordpress_4_4@ or @app_lamp_7_0@ ). Use the @get blueprints@ operation to return a list of available images (or /blueprints/ ).
-- * 'bundleId' - The bundle of specification information for your virtual private server (or /instance/ ), including the pricing plan (e.g., @micro_1_0@ ).
-- * 'customImageName' - (Deprecated) The name for your custom image.
-- * 'instanceNames' - The names to use for your new Lightsail instances. Separate multiple values using quotation marks and commas, for example: @["MyFirstInstance","MySecondInstance"]@
-- * 'keyPairName' - The name of your key pair.
-- * 'tags' - The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
-- * 'userData' - A launch script you can create that configures a server with additional user data. For example, you might want to run @apt-get -y update@ .
mkCreateInstances ::
  -- | 'availabilityZone'
  Lude.Text ->
  -- | 'blueprintId'
  Lude.Text ->
  -- | 'bundleId'
  Lude.Text ->
  CreateInstances
mkCreateInstances pAvailabilityZone_ pBlueprintId_ pBundleId_ =
  CreateInstances'
    { customImageName = Lude.Nothing,
      addOns = Lude.Nothing,
      userData = Lude.Nothing,
      keyPairName = Lude.Nothing,
      tags = Lude.Nothing,
      instanceNames = Lude.mempty,
      availabilityZone = pAvailabilityZone_,
      blueprintId = pBlueprintId_,
      bundleId = pBundleId_
    }

-- | (Deprecated) The name for your custom image.
--
-- /Note:/ Consider using 'customImageName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciCustomImageName :: Lens.Lens' CreateInstances (Lude.Maybe Lude.Text)
ciCustomImageName = Lens.lens (customImageName :: CreateInstances -> Lude.Maybe Lude.Text) (\s a -> s {customImageName = a} :: CreateInstances)
{-# DEPRECATED ciCustomImageName "Use generic-lens or generic-optics with 'customImageName' instead." #-}

-- | An array of objects representing the add-ons to enable for the new instance.
--
-- /Note:/ Consider using 'addOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAddOns :: Lens.Lens' CreateInstances (Lude.Maybe [AddOnRequest])
ciAddOns = Lens.lens (addOns :: CreateInstances -> Lude.Maybe [AddOnRequest]) (\s a -> s {addOns = a} :: CreateInstances)
{-# DEPRECATED ciAddOns "Use generic-lens or generic-optics with 'addOns' instead." #-}

-- | A launch script you can create that configures a server with additional user data. For example, you might want to run @apt-get -y update@ .
--
-- /Note:/ Consider using 'userData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciUserData :: Lens.Lens' CreateInstances (Lude.Maybe Lude.Text)
ciUserData = Lens.lens (userData :: CreateInstances -> Lude.Maybe Lude.Text) (\s a -> s {userData = a} :: CreateInstances)
{-# DEPRECATED ciUserData "Use generic-lens or generic-optics with 'userData' instead." #-}

-- | The name of your key pair.
--
-- /Note:/ Consider using 'keyPairName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciKeyPairName :: Lens.Lens' CreateInstances (Lude.Maybe Lude.Text)
ciKeyPairName = Lens.lens (keyPairName :: CreateInstances -> Lude.Maybe Lude.Text) (\s a -> s {keyPairName = a} :: CreateInstances)
{-# DEPRECATED ciKeyPairName "Use generic-lens or generic-optics with 'keyPairName' instead." #-}

-- | The tag keys and optional values to add to the resource during create.
--
-- Use the @TagResource@ action to tag a resource after it's created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciTags :: Lens.Lens' CreateInstances (Lude.Maybe [Tag])
ciTags = Lens.lens (tags :: CreateInstances -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateInstances)
{-# DEPRECATED ciTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The names to use for your new Lightsail instances. Separate multiple values using quotation marks and commas, for example: @["MyFirstInstance","MySecondInstance"]@
--
-- /Note:/ Consider using 'instanceNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciInstanceNames :: Lens.Lens' CreateInstances [Lude.Text]
ciInstanceNames = Lens.lens (instanceNames :: CreateInstances -> [Lude.Text]) (\s a -> s {instanceNames = a} :: CreateInstances)
{-# DEPRECATED ciInstanceNames "Use generic-lens or generic-optics with 'instanceNames' instead." #-}

-- | The Availability Zone in which to create your instance. Use the following format: @us-east-2a@ (case sensitive). You can get a list of Availability Zones by using the <http://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetRegions.html get regions> operation. Be sure to add the @include Availability Zones@ parameter to your request.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAvailabilityZone :: Lens.Lens' CreateInstances Lude.Text
ciAvailabilityZone = Lens.lens (availabilityZone :: CreateInstances -> Lude.Text) (\s a -> s {availabilityZone = a} :: CreateInstances)
{-# DEPRECATED ciAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | The ID for a virtual private server image (e.g., @app_wordpress_4_4@ or @app_lamp_7_0@ ). Use the @get blueprints@ operation to return a list of available images (or /blueprints/ ).
--
-- /Note:/ Consider using 'blueprintId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciBlueprintId :: Lens.Lens' CreateInstances Lude.Text
ciBlueprintId = Lens.lens (blueprintId :: CreateInstances -> Lude.Text) (\s a -> s {blueprintId = a} :: CreateInstances)
{-# DEPRECATED ciBlueprintId "Use generic-lens or generic-optics with 'blueprintId' instead." #-}

-- | The bundle of specification information for your virtual private server (or /instance/ ), including the pricing plan (e.g., @micro_1_0@ ).
--
-- /Note:/ Consider using 'bundleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciBundleId :: Lens.Lens' CreateInstances Lude.Text
ciBundleId = Lens.lens (bundleId :: CreateInstances -> Lude.Text) (\s a -> s {bundleId = a} :: CreateInstances)
{-# DEPRECATED ciBundleId "Use generic-lens or generic-optics with 'bundleId' instead." #-}

instance Lude.AWSRequest CreateInstances where
  type Rs CreateInstances = CreateInstancesResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateInstancesResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateInstances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.CreateInstances" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateInstances where
  toJSON CreateInstances' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("customImageName" Lude..=) Lude.<$> customImageName,
            ("addOns" Lude..=) Lude.<$> addOns,
            ("userData" Lude..=) Lude.<$> userData,
            ("keyPairName" Lude..=) Lude.<$> keyPairName,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("instanceNames" Lude..= instanceNames),
            Lude.Just ("availabilityZone" Lude..= availabilityZone),
            Lude.Just ("blueprintId" Lude..= blueprintId),
            Lude.Just ("bundleId" Lude..= bundleId)
          ]
      )

instance Lude.ToPath CreateInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateInstances where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateInstancesResponse' smart constructor.
data CreateInstancesResponse = CreateInstancesResponse'
  { operations ::
      Lude.Maybe [Operation],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateInstancesResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkCreateInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateInstancesResponse
mkCreateInstancesResponse pResponseStatus_ =
  CreateInstancesResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirsOperations :: Lens.Lens' CreateInstancesResponse (Lude.Maybe [Operation])
cirsOperations = Lens.lens (operations :: CreateInstancesResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: CreateInstancesResponse)
{-# DEPRECATED cirsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirsResponseStatus :: Lens.Lens' CreateInstancesResponse Lude.Int
cirsResponseStatus = Lens.lens (responseStatus :: CreateInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateInstancesResponse)
{-# DEPRECATED cirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
