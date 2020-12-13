{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.Webhook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.Webhook
  ( Webhook (..),

    -- * Smart constructor
    mkWebhook,

    -- * Lenses
    wBranchFilter,
    wLastModifiedSecret,
    wUrl,
    wSecret,
    wFilterGroups,
    wPayloadURL,
    wBuildType,
  )
where

import Network.AWS.CodeBuild.Types.WebhookBuildType
import Network.AWS.CodeBuild.Types.WebhookFilter
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a webhook that connects repository events to a build project in AWS CodeBuild.
--
-- /See:/ 'mkWebhook' smart constructor.
data Webhook = Webhook'
  { -- | A regular expression used to determine which repository branches are built when a webhook is triggered. If the name of a branch matches the regular expression, then it is built. If @branchFilter@ is empty, then all branches are built.
    branchFilter :: Lude.Maybe Lude.Text,
    -- | A timestamp that indicates the last time a repository's secret token was modified.
    lastModifiedSecret :: Lude.Maybe Lude.Timestamp,
    -- | The URL to the webhook.
    url :: Lude.Maybe Lude.Text,
    -- | The secret token of the associated repository.
    secret :: Lude.Maybe Lude.Text,
    -- | An array of arrays of @WebhookFilter@ objects used to determine which webhooks are triggered. At least one @WebhookFilter@ in the array must specify @EVENT@ as its @type@ .
    --
    -- For a build to be triggered, at least one filter group in the @filterGroups@ array must pass. For a filter group to pass, each of its filters must pass.
    filterGroups :: Lude.Maybe [[WebhookFilter]],
    -- | The AWS CodeBuild endpoint where webhook events are sent.
    payloadURL :: Lude.Maybe Lude.Text,
    -- | Specifies the type of build this webhook will trigger.
    buildType :: Lude.Maybe WebhookBuildType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Webhook' with the minimum fields required to make a request.
--
-- * 'branchFilter' - A regular expression used to determine which repository branches are built when a webhook is triggered. If the name of a branch matches the regular expression, then it is built. If @branchFilter@ is empty, then all branches are built.
-- * 'lastModifiedSecret' - A timestamp that indicates the last time a repository's secret token was modified.
-- * 'url' - The URL to the webhook.
-- * 'secret' - The secret token of the associated repository.
-- * 'filterGroups' - An array of arrays of @WebhookFilter@ objects used to determine which webhooks are triggered. At least one @WebhookFilter@ in the array must specify @EVENT@ as its @type@ .
--
-- For a build to be triggered, at least one filter group in the @filterGroups@ array must pass. For a filter group to pass, each of its filters must pass.
-- * 'payloadURL' - The AWS CodeBuild endpoint where webhook events are sent.
-- * 'buildType' - Specifies the type of build this webhook will trigger.
mkWebhook ::
  Webhook
mkWebhook =
  Webhook'
    { branchFilter = Lude.Nothing,
      lastModifiedSecret = Lude.Nothing,
      url = Lude.Nothing,
      secret = Lude.Nothing,
      filterGroups = Lude.Nothing,
      payloadURL = Lude.Nothing,
      buildType = Lude.Nothing
    }

-- | A regular expression used to determine which repository branches are built when a webhook is triggered. If the name of a branch matches the regular expression, then it is built. If @branchFilter@ is empty, then all branches are built.
--
-- /Note:/ Consider using 'branchFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wBranchFilter :: Lens.Lens' Webhook (Lude.Maybe Lude.Text)
wBranchFilter = Lens.lens (branchFilter :: Webhook -> Lude.Maybe Lude.Text) (\s a -> s {branchFilter = a} :: Webhook)
{-# DEPRECATED wBranchFilter "Use generic-lens or generic-optics with 'branchFilter' instead." #-}

-- | A timestamp that indicates the last time a repository's secret token was modified.
--
-- /Note:/ Consider using 'lastModifiedSecret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wLastModifiedSecret :: Lens.Lens' Webhook (Lude.Maybe Lude.Timestamp)
wLastModifiedSecret = Lens.lens (lastModifiedSecret :: Webhook -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedSecret = a} :: Webhook)
{-# DEPRECATED wLastModifiedSecret "Use generic-lens or generic-optics with 'lastModifiedSecret' instead." #-}

-- | The URL to the webhook.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wUrl :: Lens.Lens' Webhook (Lude.Maybe Lude.Text)
wUrl = Lens.lens (url :: Webhook -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: Webhook)
{-# DEPRECATED wUrl "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The secret token of the associated repository.
--
-- /Note:/ Consider using 'secret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wSecret :: Lens.Lens' Webhook (Lude.Maybe Lude.Text)
wSecret = Lens.lens (secret :: Webhook -> Lude.Maybe Lude.Text) (\s a -> s {secret = a} :: Webhook)
{-# DEPRECATED wSecret "Use generic-lens or generic-optics with 'secret' instead." #-}

-- | An array of arrays of @WebhookFilter@ objects used to determine which webhooks are triggered. At least one @WebhookFilter@ in the array must specify @EVENT@ as its @type@ .
--
-- For a build to be triggered, at least one filter group in the @filterGroups@ array must pass. For a filter group to pass, each of its filters must pass.
--
-- /Note:/ Consider using 'filterGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wFilterGroups :: Lens.Lens' Webhook (Lude.Maybe [[WebhookFilter]])
wFilterGroups = Lens.lens (filterGroups :: Webhook -> Lude.Maybe [[WebhookFilter]]) (\s a -> s {filterGroups = a} :: Webhook)
{-# DEPRECATED wFilterGroups "Use generic-lens or generic-optics with 'filterGroups' instead." #-}

-- | The AWS CodeBuild endpoint where webhook events are sent.
--
-- /Note:/ Consider using 'payloadURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wPayloadURL :: Lens.Lens' Webhook (Lude.Maybe Lude.Text)
wPayloadURL = Lens.lens (payloadURL :: Webhook -> Lude.Maybe Lude.Text) (\s a -> s {payloadURL = a} :: Webhook)
{-# DEPRECATED wPayloadURL "Use generic-lens or generic-optics with 'payloadURL' instead." #-}

-- | Specifies the type of build this webhook will trigger.
--
-- /Note:/ Consider using 'buildType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wBuildType :: Lens.Lens' Webhook (Lude.Maybe WebhookBuildType)
wBuildType = Lens.lens (buildType :: Webhook -> Lude.Maybe WebhookBuildType) (\s a -> s {buildType = a} :: Webhook)
{-# DEPRECATED wBuildType "Use generic-lens or generic-optics with 'buildType' instead." #-}

instance Lude.FromJSON Webhook where
  parseJSON =
    Lude.withObject
      "Webhook"
      ( \x ->
          Webhook'
            Lude.<$> (x Lude..:? "branchFilter")
            Lude.<*> (x Lude..:? "lastModifiedSecret")
            Lude.<*> (x Lude..:? "url")
            Lude.<*> (x Lude..:? "secret")
            Lude.<*> (x Lude..:? "filterGroups" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "payloadUrl")
            Lude.<*> (x Lude..:? "buildType")
      )
