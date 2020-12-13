{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.PutResourceAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides identifying details of the resource being migrated so that it can be associated in the Application Discovery Service repository. This association occurs asynchronously after @PutResourceAttributes@ returns.
--
-- /Important:/
--     * Keep in mind that subsequent calls to PutResourceAttributes will override previously stored attributes. For example, if it is first called with a MAC address, but later, it is desired to /add/ an IP address, it will then be required to call it with /both/ the IP and MAC addresses to prevent overriding the MAC address.
--
--
--     * Note the instructions regarding the special use case of the <https://docs.aws.amazon.com/migrationhub/latest/ug/API_PutResourceAttributes.html#migrationhub-PutResourceAttributes-request-ResourceAttributeList @ResourceAttributeList@ > parameter when specifying any "VM" related value.
module Network.AWS.MigrationHub.PutResourceAttributes
  ( -- * Creating a request
    PutResourceAttributes (..),
    mkPutResourceAttributes,

    -- ** Request lenses
    praResourceAttributeList,
    praProgressUpdateStream,
    praMigrationTaskName,
    praDryRun,

    -- * Destructuring the response
    PutResourceAttributesResponse (..),
    mkPutResourceAttributesResponse,

    -- ** Response lenses
    prarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutResourceAttributes' smart constructor.
data PutResourceAttributes = PutResourceAttributes'
  { -- | Information about the resource that is being migrated. This data will be used to map the task to a resource in the Application Discovery Service repository.
    --
    -- /Important:/
    --     * If any "VM" related value is set for a @ResourceAttribute@ object, it is required that @VM_MANAGER_ID@ , as a minimum, is always set. If @VM_MANAGER_ID@ is not set, then all "VM" fields will be discarded and "VM" fields will not be used for matching the migration task to a server in Application Discovery Service repository. See the <https://docs.aws.amazon.com/migrationhub/latest/ug/API_PutResourceAttributes.html#API_PutResourceAttributes_Examples Example> section below for a use case of specifying "VM" related values.
    --
    --
    --     * If a server you are trying to match has multiple IP or MAC addresses, you should provide as many as you know in separate type/value pairs passed to the @ResourceAttributeList@ parameter to maximize the chances of matching.
    resourceAttributeList :: Lude.NonEmpty ResourceAttribute,
    -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Lude.Text,
    -- | Unique identifier that references the migration task. /Do not store personal data in this field./
    migrationTaskName :: Lude.Text,
    -- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutResourceAttributes' with the minimum fields required to make a request.
--
-- * 'resourceAttributeList' - Information about the resource that is being migrated. This data will be used to map the task to a resource in the Application Discovery Service repository.
--
-- /Important:/
--     * If any "VM" related value is set for a @ResourceAttribute@ object, it is required that @VM_MANAGER_ID@ , as a minimum, is always set. If @VM_MANAGER_ID@ is not set, then all "VM" fields will be discarded and "VM" fields will not be used for matching the migration task to a server in Application Discovery Service repository. See the <https://docs.aws.amazon.com/migrationhub/latest/ug/API_PutResourceAttributes.html#API_PutResourceAttributes_Examples Example> section below for a use case of specifying "VM" related values.
--
--
--     * If a server you are trying to match has multiple IP or MAC addresses, you should provide as many as you know in separate type/value pairs passed to the @ResourceAttributeList@ parameter to maximize the chances of matching.
--
--
-- * 'progressUpdateStream' - The name of the ProgressUpdateStream.
-- * 'migrationTaskName' - Unique identifier that references the migration task. /Do not store personal data in this field./
-- * 'dryRun' - Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
mkPutResourceAttributes ::
  -- | 'resourceAttributeList'
  Lude.NonEmpty ResourceAttribute ->
  -- | 'progressUpdateStream'
  Lude.Text ->
  -- | 'migrationTaskName'
  Lude.Text ->
  PutResourceAttributes
mkPutResourceAttributes
  pResourceAttributeList_
  pProgressUpdateStream_
  pMigrationTaskName_ =
    PutResourceAttributes'
      { resourceAttributeList =
          pResourceAttributeList_,
        progressUpdateStream = pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_,
        dryRun = Lude.Nothing
      }

-- | Information about the resource that is being migrated. This data will be used to map the task to a resource in the Application Discovery Service repository.
--
-- /Important:/
--     * If any "VM" related value is set for a @ResourceAttribute@ object, it is required that @VM_MANAGER_ID@ , as a minimum, is always set. If @VM_MANAGER_ID@ is not set, then all "VM" fields will be discarded and "VM" fields will not be used for matching the migration task to a server in Application Discovery Service repository. See the <https://docs.aws.amazon.com/migrationhub/latest/ug/API_PutResourceAttributes.html#API_PutResourceAttributes_Examples Example> section below for a use case of specifying "VM" related values.
--
--
--     * If a server you are trying to match has multiple IP or MAC addresses, you should provide as many as you know in separate type/value pairs passed to the @ResourceAttributeList@ parameter to maximize the chances of matching.
--
--
--
-- /Note:/ Consider using 'resourceAttributeList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
praResourceAttributeList :: Lens.Lens' PutResourceAttributes (Lude.NonEmpty ResourceAttribute)
praResourceAttributeList = Lens.lens (resourceAttributeList :: PutResourceAttributes -> Lude.NonEmpty ResourceAttribute) (\s a -> s {resourceAttributeList = a} :: PutResourceAttributes)
{-# DEPRECATED praResourceAttributeList "Use generic-lens or generic-optics with 'resourceAttributeList' instead." #-}

-- | The name of the ProgressUpdateStream.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
praProgressUpdateStream :: Lens.Lens' PutResourceAttributes Lude.Text
praProgressUpdateStream = Lens.lens (progressUpdateStream :: PutResourceAttributes -> Lude.Text) (\s a -> s {progressUpdateStream = a} :: PutResourceAttributes)
{-# DEPRECATED praProgressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead." #-}

-- | Unique identifier that references the migration task. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
praMigrationTaskName :: Lens.Lens' PutResourceAttributes Lude.Text
praMigrationTaskName = Lens.lens (migrationTaskName :: PutResourceAttributes -> Lude.Text) (\s a -> s {migrationTaskName = a} :: PutResourceAttributes)
{-# DEPRECATED praMigrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead." #-}

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
praDryRun :: Lens.Lens' PutResourceAttributes (Lude.Maybe Lude.Bool)
praDryRun = Lens.lens (dryRun :: PutResourceAttributes -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: PutResourceAttributes)
{-# DEPRECATED praDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest PutResourceAttributes where
  type Rs PutResourceAttributes = PutResourceAttributesResponse
  request = Req.postJSON migrationHubService
  response =
    Res.receiveEmpty
      ( \s h x ->
          PutResourceAttributesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutResourceAttributes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSMigrationHub.PutResourceAttributes" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutResourceAttributes where
  toJSON PutResourceAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceAttributeList" Lude..= resourceAttributeList),
            Lude.Just ("ProgressUpdateStream" Lude..= progressUpdateStream),
            Lude.Just ("MigrationTaskName" Lude..= migrationTaskName),
            ("DryRun" Lude..=) Lude.<$> dryRun
          ]
      )

instance Lude.ToPath PutResourceAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery PutResourceAttributes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutResourceAttributesResponse' smart constructor.
newtype PutResourceAttributesResponse = PutResourceAttributesResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutResourceAttributesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutResourceAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutResourceAttributesResponse
mkPutResourceAttributesResponse pResponseStatus_ =
  PutResourceAttributesResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prarsResponseStatus :: Lens.Lens' PutResourceAttributesResponse Lude.Int
prarsResponseStatus = Lens.lens (responseStatus :: PutResourceAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutResourceAttributesResponse)
{-# DEPRECATED prarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
