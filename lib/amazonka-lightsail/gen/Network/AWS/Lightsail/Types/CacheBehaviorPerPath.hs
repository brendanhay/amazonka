{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.CacheBehaviorPerPath
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.CacheBehaviorPerPath where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.BehaviorEnum
import Network.AWS.Prelude

-- | Describes the per-path cache behavior of an Amazon Lightsail content delivery network (CDN) distribution.
--
--
-- A per-path cache behavior is used to override, or add an exception to, the default cache behavior of a distribution. For example, if the @cacheBehavior@ is set to @cache@ , then a per-path cache behavior can be used to specify a directory, file, or file type that your distribution will cache. Alternately, if the distribution's @cacheBehavior@ is @dont-cache@ , then a per-path cache behavior can be used to specify a directory, file, or file type that your distribution will not cache.
--
-- if the cacheBehavior's behavior is set to 'cache', then
--
--
-- /See:/ 'cacheBehaviorPerPath' smart constructor.
data CacheBehaviorPerPath = CacheBehaviorPerPath'
  { _cbppPath ::
      !(Maybe Text),
    _cbppBehavior :: !(Maybe BehaviorEnum)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CacheBehaviorPerPath' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cbppPath' - The path to a directory or file to cached, or not cache. Use an asterisk symbol to specify wildcard directories (@path/to/assets/*@ ), and file types (@*.html, *jpg, *js@ ). Directories and file paths are case-sensitive. Examples:     * Specify the following to cache all files in the document root of an Apache web server running on a Lightsail instance. @var/www/html/@      * Specify the following file to cache only the index page in the document root of an Apache web server. @var/www/html/index.html@      * Specify the following to cache only the .html files in the document root of an Apache web server. @var/www/html/*.html@      * Specify the following to cache only the .jpg, .png, and .gif files in the images sub-directory of the document root of an Apache web server. @var/www/html/images/*.jpg@  @var/www/html/images/*.png@  @var/www/html/images/*.gif@  Specify the following to cache all files in the images sub-directory of the document root of an Apache web server. @var/www/html/images/@
--
-- * 'cbppBehavior' - The cache behavior for the specified path. You can specify one of the following per-path cache behaviors:     * __@cache@ __ - This behavior caches the specified path.      * __@dont-cache@ __ - This behavior doesn't cache the specified path.
cacheBehaviorPerPath ::
  CacheBehaviorPerPath
cacheBehaviorPerPath =
  CacheBehaviorPerPath'
    { _cbppPath = Nothing,
      _cbppBehavior = Nothing
    }

-- | The path to a directory or file to cached, or not cache. Use an asterisk symbol to specify wildcard directories (@path/to/assets/*@ ), and file types (@*.html, *jpg, *js@ ). Directories and file paths are case-sensitive. Examples:     * Specify the following to cache all files in the document root of an Apache web server running on a Lightsail instance. @var/www/html/@      * Specify the following file to cache only the index page in the document root of an Apache web server. @var/www/html/index.html@      * Specify the following to cache only the .html files in the document root of an Apache web server. @var/www/html/*.html@      * Specify the following to cache only the .jpg, .png, and .gif files in the images sub-directory of the document root of an Apache web server. @var/www/html/images/*.jpg@  @var/www/html/images/*.png@  @var/www/html/images/*.gif@  Specify the following to cache all files in the images sub-directory of the document root of an Apache web server. @var/www/html/images/@
cbppPath :: Lens' CacheBehaviorPerPath (Maybe Text)
cbppPath = lens _cbppPath (\s a -> s {_cbppPath = a})

-- | The cache behavior for the specified path. You can specify one of the following per-path cache behaviors:     * __@cache@ __ - This behavior caches the specified path.      * __@dont-cache@ __ - This behavior doesn't cache the specified path.
cbppBehavior :: Lens' CacheBehaviorPerPath (Maybe BehaviorEnum)
cbppBehavior = lens _cbppBehavior (\s a -> s {_cbppBehavior = a})

instance FromJSON CacheBehaviorPerPath where
  parseJSON =
    withObject
      "CacheBehaviorPerPath"
      ( \x ->
          CacheBehaviorPerPath' <$> (x .:? "path") <*> (x .:? "behavior")
      )

instance Hashable CacheBehaviorPerPath

instance NFData CacheBehaviorPerPath

instance ToJSON CacheBehaviorPerPath where
  toJSON CacheBehaviorPerPath' {..} =
    object
      ( catMaybes
          [("path" .=) <$> _cbppPath, ("behavior" .=) <$> _cbppBehavior]
      )
