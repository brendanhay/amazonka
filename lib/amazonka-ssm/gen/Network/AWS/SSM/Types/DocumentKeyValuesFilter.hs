{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.DocumentKeyValuesFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.DocumentKeyValuesFilter
  ( DocumentKeyValuesFilter (..)
  -- * Smart constructor
  , mkDocumentKeyValuesFilter
  -- * Lenses
  , dkvfKey
  , dkvfValues
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.DocumentKeyValuesFilterValue as Types
import qualified Network.AWS.SSM.Types.Key as Types

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
  { key :: Core.Maybe Types.Key
    -- ^ The name of the filter key.
  , values :: Core.Maybe [Types.DocumentKeyValuesFilterValue]
    -- ^ The value for the filter key.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DocumentKeyValuesFilter' value with any optional fields omitted.
mkDocumentKeyValuesFilter
    :: DocumentKeyValuesFilter
mkDocumentKeyValuesFilter
  = DocumentKeyValuesFilter'{key = Core.Nothing,
                             values = Core.Nothing}

-- | The name of the filter key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkvfKey :: Lens.Lens' DocumentKeyValuesFilter (Core.Maybe Types.Key)
dkvfKey = Lens.field @"key"
{-# INLINEABLE dkvfKey #-}
{-# DEPRECATED key "Use generic-lens or generic-optics with 'key' instead"  #-}

-- | The value for the filter key.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dkvfValues :: Lens.Lens' DocumentKeyValuesFilter (Core.Maybe [Types.DocumentKeyValuesFilterValue])
dkvfValues = Lens.field @"values"
{-# INLINEABLE dkvfValues #-}
{-# DEPRECATED values "Use generic-lens or generic-optics with 'values' instead"  #-}

instance Core.FromJSON DocumentKeyValuesFilter where
        toJSON DocumentKeyValuesFilter{..}
          = Core.object
              (Core.catMaybes
                 [("Key" Core..=) Core.<$> key, ("Values" Core..=) Core.<$> values])
