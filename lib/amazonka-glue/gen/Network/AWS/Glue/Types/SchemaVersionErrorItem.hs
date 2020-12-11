-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.SchemaVersionErrorItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.SchemaVersionErrorItem
  ( SchemaVersionErrorItem (..),

    -- * Smart constructor
    mkSchemaVersionErrorItem,

    -- * Lenses
    sveiVersionNumber,
    sveiErrorDetails,
  )
where

import Network.AWS.Glue.Types.ErrorDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that contains the error details for an operation on a schema version.
--
-- /See:/ 'mkSchemaVersionErrorItem' smart constructor.
data SchemaVersionErrorItem = SchemaVersionErrorItem'
  { versionNumber ::
      Lude.Maybe Lude.Natural,
    errorDetails :: Lude.Maybe ErrorDetails
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SchemaVersionErrorItem' with the minimum fields required to make a request.
--
-- * 'errorDetails' - The details of the error for the schema version.
-- * 'versionNumber' - The version number of the schema.
mkSchemaVersionErrorItem ::
  SchemaVersionErrorItem
mkSchemaVersionErrorItem =
  SchemaVersionErrorItem'
    { versionNumber = Lude.Nothing,
      errorDetails = Lude.Nothing
    }

-- | The version number of the schema.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sveiVersionNumber :: Lens.Lens' SchemaVersionErrorItem (Lude.Maybe Lude.Natural)
sveiVersionNumber = Lens.lens (versionNumber :: SchemaVersionErrorItem -> Lude.Maybe Lude.Natural) (\s a -> s {versionNumber = a} :: SchemaVersionErrorItem)
{-# DEPRECATED sveiVersionNumber "Use generic-lens or generic-optics with 'versionNumber' instead." #-}

-- | The details of the error for the schema version.
--
-- /Note:/ Consider using 'errorDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sveiErrorDetails :: Lens.Lens' SchemaVersionErrorItem (Lude.Maybe ErrorDetails)
sveiErrorDetails = Lens.lens (errorDetails :: SchemaVersionErrorItem -> Lude.Maybe ErrorDetails) (\s a -> s {errorDetails = a} :: SchemaVersionErrorItem)
{-# DEPRECATED sveiErrorDetails "Use generic-lens or generic-optics with 'errorDetails' instead." #-}

instance Lude.FromJSON SchemaVersionErrorItem where
  parseJSON =
    Lude.withObject
      "SchemaVersionErrorItem"
      ( \x ->
          SchemaVersionErrorItem'
            Lude.<$> (x Lude..:? "VersionNumber") Lude.<*> (x Lude..:? "ErrorDetails")
      )
