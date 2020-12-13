{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentKeyValuesFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.DocumentKeyValuesFilter
  ( DocumentKeyValuesFilter (..),

    -- * Smart constructor
    mkDocumentKeyValuesFilter,

    -- * Lenses
    dkvfValues,
    dkvfKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | One or more filters. Use a filter to return a more specific list of documents.
--
-- For keys, you can specify one or more tags that have been applied to a document.
-- You can also use AWS-provided keys, some of which have specific allowed values. These keys and their associated values are as follows:
--
--     * DocumentType
--
--     *
--     * ApplicationConfiguration
--
--
--     * ApplicationConfigurationSchema
--
--
--     * Automation
--
--
--     * ChangeCalendar
--
--
--     * Command
--
--
--     * DeploymentStrategy
--
--
--     * Package
--
--
--     * Policy
--
--
--     * Session
--
--
--
--
--     * Owner
--
--     * Note that only one @Owner@ can be specified in a request. For example: @Key=Owner,Values=Self@ .
--
--     * Amazon
--
--
--     * Private
--
--
--     * Public
--
--
--     * Self
--
--
--     * ThirdParty
--
--
--
--
--     * PlatformTypes
--
--     *
--     * Linux
--
--
--     * Windows
--
--
--
--
-- @Name@ is another AWS-provided key. If you use @Name@ as a key, you can use a name prefix to return a list of documents. For example, in the AWS CLI, to return a list of all documents that begin with @Te@ , run the following command:
-- @aws ssm list-documents --filters Key=Name,Values=Te@
-- You can also use the @TargetType@ AWS-provided key. For a list of valid resource type values that can be used with this key, see <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS resource and property types reference> in the /AWS CloudFormation User Guide/ .
-- If you specify more than two keys, only documents that are identified by all the tags are returned in the results. If you specify more than two values for a key, documents that are identified by any of the values are returned in the results.
-- To specify a custom key and value pair, use the format @Key=tag:tagName,Values=valueName@ .
-- For example, if you created a key called region and are using the AWS CLI to call the @list-documents@ command:
-- @aws ssm list-documents --filters Key=tag:region,Values=east,west Key=Owner,Values=Self@
--
-- /See:/ 'mkDocumentKeyValuesFilter' smart constructor.
data DocumentKeyValuesFilter = DocumentKeyValuesFilter'
  { -- | The value for the filter key.
    values :: Lude.Maybe [Lude.Text],
    -- | The name of the filter key.
    key :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentKeyValuesFilter' with the minimum fields required to make a request.
--
-- * 'values' - The value for the filter key.
-- * 'key' - The name of the filter key.
mkDocumentKeyValuesFilter ::
  DocumentKeyValuesFilter
mkDocumentKeyValuesFilter =
  DocumentKeyValuesFilter'
    { values = Lude.Nothing,
      key = Lude.Nothing
    }

-- | The value for the filter key.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkvfValues :: Lens.Lens' DocumentKeyValuesFilter (Lude.Maybe [Lude.Text])
dkvfValues = Lens.lens (values :: DocumentKeyValuesFilter -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: DocumentKeyValuesFilter)
{-# DEPRECATED dkvfValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The name of the filter key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkvfKey :: Lens.Lens' DocumentKeyValuesFilter (Lude.Maybe Lude.Text)
dkvfKey = Lens.lens (key :: DocumentKeyValuesFilter -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: DocumentKeyValuesFilter)
{-# DEPRECATED dkvfKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.ToJSON DocumentKeyValuesFilter where
  toJSON DocumentKeyValuesFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Values" Lude..=) Lude.<$> values, ("Key" Lude..=) Lude.<$> key]
      )
