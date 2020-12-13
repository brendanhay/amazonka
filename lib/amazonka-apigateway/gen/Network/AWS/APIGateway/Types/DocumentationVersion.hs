{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.DocumentationVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.DocumentationVersion
  ( DocumentationVersion (..),

    -- * Smart constructor
    mkDocumentationVersion,

    -- * Lenses
    dvCreatedDate,
    dvVersion,
    dvDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A snapshot of the documentation of an API.
--
-- Publishing API documentation involves creating a documentation version associated with an API stage and exporting the versioned documentation to an external (e.g., OpenAPI) file.
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-documenting-api.html Documenting an API> , 'DocumentationPart' , 'DocumentationVersions'
--
-- /See:/ 'mkDocumentationVersion' smart constructor.
data DocumentationVersion = DocumentationVersion'
  { -- | The date when the API documentation snapshot is created.
    createdDate :: Lude.Maybe Lude.Timestamp,
    -- | The version identifier of the API documentation snapshot.
    version :: Lude.Maybe Lude.Text,
    -- | The description of the API documentation snapshot.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentationVersion' with the minimum fields required to make a request.
--
-- * 'createdDate' - The date when the API documentation snapshot is created.
-- * 'version' - The version identifier of the API documentation snapshot.
-- * 'description' - The description of the API documentation snapshot.
mkDocumentationVersion ::
  DocumentationVersion
mkDocumentationVersion =
  DocumentationVersion'
    { createdDate = Lude.Nothing,
      version = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The date when the API documentation snapshot is created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvCreatedDate :: Lens.Lens' DocumentationVersion (Lude.Maybe Lude.Timestamp)
dvCreatedDate = Lens.lens (createdDate :: DocumentationVersion -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: DocumentationVersion)
{-# DEPRECATED dvCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The version identifier of the API documentation snapshot.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvVersion :: Lens.Lens' DocumentationVersion (Lude.Maybe Lude.Text)
dvVersion = Lens.lens (version :: DocumentationVersion -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: DocumentationVersion)
{-# DEPRECATED dvVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The description of the API documentation snapshot.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvDescription :: Lens.Lens' DocumentationVersion (Lude.Maybe Lude.Text)
dvDescription = Lens.lens (description :: DocumentationVersion -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DocumentationVersion)
{-# DEPRECATED dvDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON DocumentationVersion where
  parseJSON =
    Lude.withObject
      "DocumentationVersion"
      ( \x ->
          DocumentationVersion'
            Lude.<$> (x Lude..:? "createdDate")
            Lude.<*> (x Lude..:? "version")
            Lude.<*> (x Lude..:? "description")
      )
