-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.GeoMatchConstraintValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.GeoMatchConstraintValue
  ( GeoMatchConstraintValue
      ( GeoMatchConstraintValue',
        GMCVAD,
        GMCVAE,
        GMCVAF,
        GMCVAG,
        GMCVAI,
        GMCVAL,
        GMCVAM,
        GMCVAO,
        GMCVAQ,
        GMCVAR,
        GMCVAS,
        GMCVAT,
        GMCVAU,
        GMCVAW,
        GMCVAX,
        GMCVAZ,
        GMCVBA,
        GMCVBB,
        GMCVBD,
        GMCVBE,
        GMCVBF,
        GMCVBG,
        GMCVBH,
        GMCVBI,
        GMCVBJ,
        GMCVBL,
        GMCVBM,
        GMCVBN,
        GMCVBO,
        GMCVBQ,
        GMCVBR,
        GMCVBS,
        GMCVBT,
        GMCVBV,
        GMCVBW,
        GMCVBY,
        GMCVBZ,
        GMCVCA,
        GMCVCC,
        GMCVCD,
        GMCVCF,
        GMCVCG,
        GMCVCH,
        GMCVCI,
        GMCVCK,
        GMCVCL,
        GMCVCM,
        GMCVCN,
        GMCVCO,
        GMCVCR,
        GMCVCU,
        GMCVCV,
        GMCVCW,
        GMCVCX,
        GMCVCY,
        GMCVCZ,
        GMCVDE,
        GMCVDJ,
        GMCVDK,
        GMCVDM,
        GMCVDO,
        GMCVDZ,
        GMCVEC,
        GMCVEE,
        GMCVEG,
        GMCVEH,
        GMCVER,
        GMCVES,
        GMCVET,
        GMCVFI,
        GMCVFJ,
        GMCVFK,
        GMCVFM,
        GMCVFO,
        GMCVFR,
        GMCVGA,
        GMCVGB,
        GMCVGD,
        GMCVGE,
        GMCVGF,
        GMCVGG,
        GMCVGH,
        GMCVGI,
        GMCVGL,
        GMCVGM,
        GMCVGN,
        GMCVGP,
        GMCVGQ,
        GMCVGR,
        GMCVGS,
        GMCVGT,
        GMCVGU,
        GMCVGW,
        GMCVGY,
        GMCVHK,
        GMCVHM,
        GMCVHN,
        GMCVHR,
        GMCVHT,
        GMCVHU,
        GMCVIE,
        GMCVIL,
        GMCVIM,
        GMCVIN,
        GMCVIO,
        GMCVIQ,
        GMCVIR,
        GMCVIS,
        GMCVIT,
        GMCVId,
        GMCVJE,
        GMCVJM,
        GMCVJO,
        GMCVJP,
        GMCVKE,
        GMCVKG,
        GMCVKH,
        GMCVKI,
        GMCVKM,
        GMCVKN,
        GMCVKP,
        GMCVKR,
        GMCVKW,
        GMCVKY,
        GMCVKZ,
        GMCVLA,
        GMCVLB,
        GMCVLC,
        GMCVLI,
        GMCVLK,
        GMCVLR,
        GMCVLS,
        GMCVLT,
        GMCVLU,
        GMCVLV,
        GMCVLY,
        GMCVMA,
        GMCVMC,
        GMCVMD,
        GMCVME,
        GMCVMF,
        GMCVMG,
        GMCVMH,
        GMCVMK,
        GMCVML,
        GMCVMM,
        GMCVMN,
        GMCVMO,
        GMCVMP,
        GMCVMQ,
        GMCVMR,
        GMCVMS,
        GMCVMT,
        GMCVMU,
        GMCVMV,
        GMCVMW,
        GMCVMX,
        GMCVMY,
        GMCVMZ,
        GMCVNA,
        GMCVNC,
        GMCVNE,
        GMCVNF,
        GMCVNG,
        GMCVNI,
        GMCVNL,
        GMCVNO,
        GMCVNP,
        GMCVNR,
        GMCVNU,
        GMCVNZ,
        GMCVOM,
        GMCVPA,
        GMCVPE,
        GMCVPF,
        GMCVPG,
        GMCVPH,
        GMCVPK,
        GMCVPL,
        GMCVPM,
        GMCVPN,
        GMCVPR,
        GMCVPS,
        GMCVPT,
        GMCVPW,
        GMCVPY,
        GMCVQA,
        GMCVRE,
        GMCVRO,
        GMCVRS,
        GMCVRU,
        GMCVRW,
        GMCVSA,
        GMCVSB,
        GMCVSC,
        GMCVSD,
        GMCVSE,
        GMCVSG,
        GMCVSH,
        GMCVSI,
        GMCVSJ,
        GMCVSK,
        GMCVSL,
        GMCVSM,
        GMCVSN,
        GMCVSO,
        GMCVSR,
        GMCVSS,
        GMCVST,
        GMCVSV,
        GMCVSX,
        GMCVSY,
        GMCVSZ,
        GMCVTC,
        GMCVTD,
        GMCVTF,
        GMCVTG,
        GMCVTH,
        GMCVTJ,
        GMCVTK,
        GMCVTL,
        GMCVTM,
        GMCVTN,
        GMCVTO,
        GMCVTR,
        GMCVTT,
        GMCVTV,
        GMCVTW,
        GMCVTZ,
        GMCVUA,
        GMCVUG,
        GMCVUM,
        GMCVUS,
        GMCVUY,
        GMCVUZ,
        GMCVVA,
        GMCVVC,
        GMCVVE,
        GMCVVG,
        GMCVVI,
        GMCVVN,
        GMCVVU,
        GMCVWF,
        GMCVWS,
        GMCVYE,
        GMCVYT,
        GMCVZA,
        GMCVZM,
        GMCVZW
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype GeoMatchConstraintValue = GeoMatchConstraintValue' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern GMCVAD :: GeoMatchConstraintValue
pattern GMCVAD = GeoMatchConstraintValue' "AD"

pattern GMCVAE :: GeoMatchConstraintValue
pattern GMCVAE = GeoMatchConstraintValue' "AE"

pattern GMCVAF :: GeoMatchConstraintValue
pattern GMCVAF = GeoMatchConstraintValue' "AF"

pattern GMCVAG :: GeoMatchConstraintValue
pattern GMCVAG = GeoMatchConstraintValue' "AG"

pattern GMCVAI :: GeoMatchConstraintValue
pattern GMCVAI = GeoMatchConstraintValue' "AI"

pattern GMCVAL :: GeoMatchConstraintValue
pattern GMCVAL = GeoMatchConstraintValue' "AL"

pattern GMCVAM :: GeoMatchConstraintValue
pattern GMCVAM = GeoMatchConstraintValue' "AM"

pattern GMCVAO :: GeoMatchConstraintValue
pattern GMCVAO = GeoMatchConstraintValue' "AO"

pattern GMCVAQ :: GeoMatchConstraintValue
pattern GMCVAQ = GeoMatchConstraintValue' "AQ"

pattern GMCVAR :: GeoMatchConstraintValue
pattern GMCVAR = GeoMatchConstraintValue' "AR"

pattern GMCVAS :: GeoMatchConstraintValue
pattern GMCVAS = GeoMatchConstraintValue' "AS"

pattern GMCVAT :: GeoMatchConstraintValue
pattern GMCVAT = GeoMatchConstraintValue' "AT"

pattern GMCVAU :: GeoMatchConstraintValue
pattern GMCVAU = GeoMatchConstraintValue' "AU"

pattern GMCVAW :: GeoMatchConstraintValue
pattern GMCVAW = GeoMatchConstraintValue' "AW"

pattern GMCVAX :: GeoMatchConstraintValue
pattern GMCVAX = GeoMatchConstraintValue' "AX"

pattern GMCVAZ :: GeoMatchConstraintValue
pattern GMCVAZ = GeoMatchConstraintValue' "AZ"

pattern GMCVBA :: GeoMatchConstraintValue
pattern GMCVBA = GeoMatchConstraintValue' "BA"

pattern GMCVBB :: GeoMatchConstraintValue
pattern GMCVBB = GeoMatchConstraintValue' "BB"

pattern GMCVBD :: GeoMatchConstraintValue
pattern GMCVBD = GeoMatchConstraintValue' "BD"

pattern GMCVBE :: GeoMatchConstraintValue
pattern GMCVBE = GeoMatchConstraintValue' "BE"

pattern GMCVBF :: GeoMatchConstraintValue
pattern GMCVBF = GeoMatchConstraintValue' "BF"

pattern GMCVBG :: GeoMatchConstraintValue
pattern GMCVBG = GeoMatchConstraintValue' "BG"

pattern GMCVBH :: GeoMatchConstraintValue
pattern GMCVBH = GeoMatchConstraintValue' "BH"

pattern GMCVBI :: GeoMatchConstraintValue
pattern GMCVBI = GeoMatchConstraintValue' "BI"

pattern GMCVBJ :: GeoMatchConstraintValue
pattern GMCVBJ = GeoMatchConstraintValue' "BJ"

pattern GMCVBL :: GeoMatchConstraintValue
pattern GMCVBL = GeoMatchConstraintValue' "BL"

pattern GMCVBM :: GeoMatchConstraintValue
pattern GMCVBM = GeoMatchConstraintValue' "BM"

pattern GMCVBN :: GeoMatchConstraintValue
pattern GMCVBN = GeoMatchConstraintValue' "BN"

pattern GMCVBO :: GeoMatchConstraintValue
pattern GMCVBO = GeoMatchConstraintValue' "BO"

pattern GMCVBQ :: GeoMatchConstraintValue
pattern GMCVBQ = GeoMatchConstraintValue' "BQ"

pattern GMCVBR :: GeoMatchConstraintValue
pattern GMCVBR = GeoMatchConstraintValue' "BR"

pattern GMCVBS :: GeoMatchConstraintValue
pattern GMCVBS = GeoMatchConstraintValue' "BS"

pattern GMCVBT :: GeoMatchConstraintValue
pattern GMCVBT = GeoMatchConstraintValue' "BT"

pattern GMCVBV :: GeoMatchConstraintValue
pattern GMCVBV = GeoMatchConstraintValue' "BV"

pattern GMCVBW :: GeoMatchConstraintValue
pattern GMCVBW = GeoMatchConstraintValue' "BW"

pattern GMCVBY :: GeoMatchConstraintValue
pattern GMCVBY = GeoMatchConstraintValue' "BY"

pattern GMCVBZ :: GeoMatchConstraintValue
pattern GMCVBZ = GeoMatchConstraintValue' "BZ"

pattern GMCVCA :: GeoMatchConstraintValue
pattern GMCVCA = GeoMatchConstraintValue' "CA"

pattern GMCVCC :: GeoMatchConstraintValue
pattern GMCVCC = GeoMatchConstraintValue' "CC"

pattern GMCVCD :: GeoMatchConstraintValue
pattern GMCVCD = GeoMatchConstraintValue' "CD"

pattern GMCVCF :: GeoMatchConstraintValue
pattern GMCVCF = GeoMatchConstraintValue' "CF"

pattern GMCVCG :: GeoMatchConstraintValue
pattern GMCVCG = GeoMatchConstraintValue' "CG"

pattern GMCVCH :: GeoMatchConstraintValue
pattern GMCVCH = GeoMatchConstraintValue' "CH"

pattern GMCVCI :: GeoMatchConstraintValue
pattern GMCVCI = GeoMatchConstraintValue' "CI"

pattern GMCVCK :: GeoMatchConstraintValue
pattern GMCVCK = GeoMatchConstraintValue' "CK"

pattern GMCVCL :: GeoMatchConstraintValue
pattern GMCVCL = GeoMatchConstraintValue' "CL"

pattern GMCVCM :: GeoMatchConstraintValue
pattern GMCVCM = GeoMatchConstraintValue' "CM"

pattern GMCVCN :: GeoMatchConstraintValue
pattern GMCVCN = GeoMatchConstraintValue' "CN"

pattern GMCVCO :: GeoMatchConstraintValue
pattern GMCVCO = GeoMatchConstraintValue' "CO"

pattern GMCVCR :: GeoMatchConstraintValue
pattern GMCVCR = GeoMatchConstraintValue' "CR"

pattern GMCVCU :: GeoMatchConstraintValue
pattern GMCVCU = GeoMatchConstraintValue' "CU"

pattern GMCVCV :: GeoMatchConstraintValue
pattern GMCVCV = GeoMatchConstraintValue' "CV"

pattern GMCVCW :: GeoMatchConstraintValue
pattern GMCVCW = GeoMatchConstraintValue' "CW"

pattern GMCVCX :: GeoMatchConstraintValue
pattern GMCVCX = GeoMatchConstraintValue' "CX"

pattern GMCVCY :: GeoMatchConstraintValue
pattern GMCVCY = GeoMatchConstraintValue' "CY"

pattern GMCVCZ :: GeoMatchConstraintValue
pattern GMCVCZ = GeoMatchConstraintValue' "CZ"

pattern GMCVDE :: GeoMatchConstraintValue
pattern GMCVDE = GeoMatchConstraintValue' "DE"

pattern GMCVDJ :: GeoMatchConstraintValue
pattern GMCVDJ = GeoMatchConstraintValue' "DJ"

pattern GMCVDK :: GeoMatchConstraintValue
pattern GMCVDK = GeoMatchConstraintValue' "DK"

pattern GMCVDM :: GeoMatchConstraintValue
pattern GMCVDM = GeoMatchConstraintValue' "DM"

pattern GMCVDO :: GeoMatchConstraintValue
pattern GMCVDO = GeoMatchConstraintValue' "DO"

pattern GMCVDZ :: GeoMatchConstraintValue
pattern GMCVDZ = GeoMatchConstraintValue' "DZ"

pattern GMCVEC :: GeoMatchConstraintValue
pattern GMCVEC = GeoMatchConstraintValue' "EC"

pattern GMCVEE :: GeoMatchConstraintValue
pattern GMCVEE = GeoMatchConstraintValue' "EE"

pattern GMCVEG :: GeoMatchConstraintValue
pattern GMCVEG = GeoMatchConstraintValue' "EG"

pattern GMCVEH :: GeoMatchConstraintValue
pattern GMCVEH = GeoMatchConstraintValue' "EH"

pattern GMCVER :: GeoMatchConstraintValue
pattern GMCVER = GeoMatchConstraintValue' "ER"

pattern GMCVES :: GeoMatchConstraintValue
pattern GMCVES = GeoMatchConstraintValue' "ES"

pattern GMCVET :: GeoMatchConstraintValue
pattern GMCVET = GeoMatchConstraintValue' "ET"

pattern GMCVFI :: GeoMatchConstraintValue
pattern GMCVFI = GeoMatchConstraintValue' "FI"

pattern GMCVFJ :: GeoMatchConstraintValue
pattern GMCVFJ = GeoMatchConstraintValue' "FJ"

pattern GMCVFK :: GeoMatchConstraintValue
pattern GMCVFK = GeoMatchConstraintValue' "FK"

pattern GMCVFM :: GeoMatchConstraintValue
pattern GMCVFM = GeoMatchConstraintValue' "FM"

pattern GMCVFO :: GeoMatchConstraintValue
pattern GMCVFO = GeoMatchConstraintValue' "FO"

pattern GMCVFR :: GeoMatchConstraintValue
pattern GMCVFR = GeoMatchConstraintValue' "FR"

pattern GMCVGA :: GeoMatchConstraintValue
pattern GMCVGA = GeoMatchConstraintValue' "GA"

pattern GMCVGB :: GeoMatchConstraintValue
pattern GMCVGB = GeoMatchConstraintValue' "GB"

pattern GMCVGD :: GeoMatchConstraintValue
pattern GMCVGD = GeoMatchConstraintValue' "GD"

pattern GMCVGE :: GeoMatchConstraintValue
pattern GMCVGE = GeoMatchConstraintValue' "GE"

pattern GMCVGF :: GeoMatchConstraintValue
pattern GMCVGF = GeoMatchConstraintValue' "GF"

pattern GMCVGG :: GeoMatchConstraintValue
pattern GMCVGG = GeoMatchConstraintValue' "GG"

pattern GMCVGH :: GeoMatchConstraintValue
pattern GMCVGH = GeoMatchConstraintValue' "GH"

pattern GMCVGI :: GeoMatchConstraintValue
pattern GMCVGI = GeoMatchConstraintValue' "GI"

pattern GMCVGL :: GeoMatchConstraintValue
pattern GMCVGL = GeoMatchConstraintValue' "GL"

pattern GMCVGM :: GeoMatchConstraintValue
pattern GMCVGM = GeoMatchConstraintValue' "GM"

pattern GMCVGN :: GeoMatchConstraintValue
pattern GMCVGN = GeoMatchConstraintValue' "GN"

pattern GMCVGP :: GeoMatchConstraintValue
pattern GMCVGP = GeoMatchConstraintValue' "GP"

pattern GMCVGQ :: GeoMatchConstraintValue
pattern GMCVGQ = GeoMatchConstraintValue' "GQ"

pattern GMCVGR :: GeoMatchConstraintValue
pattern GMCVGR = GeoMatchConstraintValue' "GR"

pattern GMCVGS :: GeoMatchConstraintValue
pattern GMCVGS = GeoMatchConstraintValue' "GS"

pattern GMCVGT :: GeoMatchConstraintValue
pattern GMCVGT = GeoMatchConstraintValue' "GT"

pattern GMCVGU :: GeoMatchConstraintValue
pattern GMCVGU = GeoMatchConstraintValue' "GU"

pattern GMCVGW :: GeoMatchConstraintValue
pattern GMCVGW = GeoMatchConstraintValue' "GW"

pattern GMCVGY :: GeoMatchConstraintValue
pattern GMCVGY = GeoMatchConstraintValue' "GY"

pattern GMCVHK :: GeoMatchConstraintValue
pattern GMCVHK = GeoMatchConstraintValue' "HK"

pattern GMCVHM :: GeoMatchConstraintValue
pattern GMCVHM = GeoMatchConstraintValue' "HM"

pattern GMCVHN :: GeoMatchConstraintValue
pattern GMCVHN = GeoMatchConstraintValue' "HN"

pattern GMCVHR :: GeoMatchConstraintValue
pattern GMCVHR = GeoMatchConstraintValue' "HR"

pattern GMCVHT :: GeoMatchConstraintValue
pattern GMCVHT = GeoMatchConstraintValue' "HT"

pattern GMCVHU :: GeoMatchConstraintValue
pattern GMCVHU = GeoMatchConstraintValue' "HU"

pattern GMCVIE :: GeoMatchConstraintValue
pattern GMCVIE = GeoMatchConstraintValue' "IE"

pattern GMCVIL :: GeoMatchConstraintValue
pattern GMCVIL = GeoMatchConstraintValue' "IL"

pattern GMCVIM :: GeoMatchConstraintValue
pattern GMCVIM = GeoMatchConstraintValue' "IM"

pattern GMCVIN :: GeoMatchConstraintValue
pattern GMCVIN = GeoMatchConstraintValue' "IN"

pattern GMCVIO :: GeoMatchConstraintValue
pattern GMCVIO = GeoMatchConstraintValue' "IO"

pattern GMCVIQ :: GeoMatchConstraintValue
pattern GMCVIQ = GeoMatchConstraintValue' "IQ"

pattern GMCVIR :: GeoMatchConstraintValue
pattern GMCVIR = GeoMatchConstraintValue' "IR"

pattern GMCVIS :: GeoMatchConstraintValue
pattern GMCVIS = GeoMatchConstraintValue' "IS"

pattern GMCVIT :: GeoMatchConstraintValue
pattern GMCVIT = GeoMatchConstraintValue' "IT"

pattern GMCVId :: GeoMatchConstraintValue
pattern GMCVId = GeoMatchConstraintValue' "ID"

pattern GMCVJE :: GeoMatchConstraintValue
pattern GMCVJE = GeoMatchConstraintValue' "JE"

pattern GMCVJM :: GeoMatchConstraintValue
pattern GMCVJM = GeoMatchConstraintValue' "JM"

pattern GMCVJO :: GeoMatchConstraintValue
pattern GMCVJO = GeoMatchConstraintValue' "JO"

pattern GMCVJP :: GeoMatchConstraintValue
pattern GMCVJP = GeoMatchConstraintValue' "JP"

pattern GMCVKE :: GeoMatchConstraintValue
pattern GMCVKE = GeoMatchConstraintValue' "KE"

pattern GMCVKG :: GeoMatchConstraintValue
pattern GMCVKG = GeoMatchConstraintValue' "KG"

pattern GMCVKH :: GeoMatchConstraintValue
pattern GMCVKH = GeoMatchConstraintValue' "KH"

pattern GMCVKI :: GeoMatchConstraintValue
pattern GMCVKI = GeoMatchConstraintValue' "KI"

pattern GMCVKM :: GeoMatchConstraintValue
pattern GMCVKM = GeoMatchConstraintValue' "KM"

pattern GMCVKN :: GeoMatchConstraintValue
pattern GMCVKN = GeoMatchConstraintValue' "KN"

pattern GMCVKP :: GeoMatchConstraintValue
pattern GMCVKP = GeoMatchConstraintValue' "KP"

pattern GMCVKR :: GeoMatchConstraintValue
pattern GMCVKR = GeoMatchConstraintValue' "KR"

pattern GMCVKW :: GeoMatchConstraintValue
pattern GMCVKW = GeoMatchConstraintValue' "KW"

pattern GMCVKY :: GeoMatchConstraintValue
pattern GMCVKY = GeoMatchConstraintValue' "KY"

pattern GMCVKZ :: GeoMatchConstraintValue
pattern GMCVKZ = GeoMatchConstraintValue' "KZ"

pattern GMCVLA :: GeoMatchConstraintValue
pattern GMCVLA = GeoMatchConstraintValue' "LA"

pattern GMCVLB :: GeoMatchConstraintValue
pattern GMCVLB = GeoMatchConstraintValue' "LB"

pattern GMCVLC :: GeoMatchConstraintValue
pattern GMCVLC = GeoMatchConstraintValue' "LC"

pattern GMCVLI :: GeoMatchConstraintValue
pattern GMCVLI = GeoMatchConstraintValue' "LI"

pattern GMCVLK :: GeoMatchConstraintValue
pattern GMCVLK = GeoMatchConstraintValue' "LK"

pattern GMCVLR :: GeoMatchConstraintValue
pattern GMCVLR = GeoMatchConstraintValue' "LR"

pattern GMCVLS :: GeoMatchConstraintValue
pattern GMCVLS = GeoMatchConstraintValue' "LS"

pattern GMCVLT :: GeoMatchConstraintValue
pattern GMCVLT = GeoMatchConstraintValue' "LT"

pattern GMCVLU :: GeoMatchConstraintValue
pattern GMCVLU = GeoMatchConstraintValue' "LU"

pattern GMCVLV :: GeoMatchConstraintValue
pattern GMCVLV = GeoMatchConstraintValue' "LV"

pattern GMCVLY :: GeoMatchConstraintValue
pattern GMCVLY = GeoMatchConstraintValue' "LY"

pattern GMCVMA :: GeoMatchConstraintValue
pattern GMCVMA = GeoMatchConstraintValue' "MA"

pattern GMCVMC :: GeoMatchConstraintValue
pattern GMCVMC = GeoMatchConstraintValue' "MC"

pattern GMCVMD :: GeoMatchConstraintValue
pattern GMCVMD = GeoMatchConstraintValue' "MD"

pattern GMCVME :: GeoMatchConstraintValue
pattern GMCVME = GeoMatchConstraintValue' "ME"

pattern GMCVMF :: GeoMatchConstraintValue
pattern GMCVMF = GeoMatchConstraintValue' "MF"

pattern GMCVMG :: GeoMatchConstraintValue
pattern GMCVMG = GeoMatchConstraintValue' "MG"

pattern GMCVMH :: GeoMatchConstraintValue
pattern GMCVMH = GeoMatchConstraintValue' "MH"

pattern GMCVMK :: GeoMatchConstraintValue
pattern GMCVMK = GeoMatchConstraintValue' "MK"

pattern GMCVML :: GeoMatchConstraintValue
pattern GMCVML = GeoMatchConstraintValue' "ML"

pattern GMCVMM :: GeoMatchConstraintValue
pattern GMCVMM = GeoMatchConstraintValue' "MM"

pattern GMCVMN :: GeoMatchConstraintValue
pattern GMCVMN = GeoMatchConstraintValue' "MN"

pattern GMCVMO :: GeoMatchConstraintValue
pattern GMCVMO = GeoMatchConstraintValue' "MO"

pattern GMCVMP :: GeoMatchConstraintValue
pattern GMCVMP = GeoMatchConstraintValue' "MP"

pattern GMCVMQ :: GeoMatchConstraintValue
pattern GMCVMQ = GeoMatchConstraintValue' "MQ"

pattern GMCVMR :: GeoMatchConstraintValue
pattern GMCVMR = GeoMatchConstraintValue' "MR"

pattern GMCVMS :: GeoMatchConstraintValue
pattern GMCVMS = GeoMatchConstraintValue' "MS"

pattern GMCVMT :: GeoMatchConstraintValue
pattern GMCVMT = GeoMatchConstraintValue' "MT"

pattern GMCVMU :: GeoMatchConstraintValue
pattern GMCVMU = GeoMatchConstraintValue' "MU"

pattern GMCVMV :: GeoMatchConstraintValue
pattern GMCVMV = GeoMatchConstraintValue' "MV"

pattern GMCVMW :: GeoMatchConstraintValue
pattern GMCVMW = GeoMatchConstraintValue' "MW"

pattern GMCVMX :: GeoMatchConstraintValue
pattern GMCVMX = GeoMatchConstraintValue' "MX"

pattern GMCVMY :: GeoMatchConstraintValue
pattern GMCVMY = GeoMatchConstraintValue' "MY"

pattern GMCVMZ :: GeoMatchConstraintValue
pattern GMCVMZ = GeoMatchConstraintValue' "MZ"

pattern GMCVNA :: GeoMatchConstraintValue
pattern GMCVNA = GeoMatchConstraintValue' "NA"

pattern GMCVNC :: GeoMatchConstraintValue
pattern GMCVNC = GeoMatchConstraintValue' "NC"

pattern GMCVNE :: GeoMatchConstraintValue
pattern GMCVNE = GeoMatchConstraintValue' "NE"

pattern GMCVNF :: GeoMatchConstraintValue
pattern GMCVNF = GeoMatchConstraintValue' "NF"

pattern GMCVNG :: GeoMatchConstraintValue
pattern GMCVNG = GeoMatchConstraintValue' "NG"

pattern GMCVNI :: GeoMatchConstraintValue
pattern GMCVNI = GeoMatchConstraintValue' "NI"

pattern GMCVNL :: GeoMatchConstraintValue
pattern GMCVNL = GeoMatchConstraintValue' "NL"

pattern GMCVNO :: GeoMatchConstraintValue
pattern GMCVNO = GeoMatchConstraintValue' "NO"

pattern GMCVNP :: GeoMatchConstraintValue
pattern GMCVNP = GeoMatchConstraintValue' "NP"

pattern GMCVNR :: GeoMatchConstraintValue
pattern GMCVNR = GeoMatchConstraintValue' "NR"

pattern GMCVNU :: GeoMatchConstraintValue
pattern GMCVNU = GeoMatchConstraintValue' "NU"

pattern GMCVNZ :: GeoMatchConstraintValue
pattern GMCVNZ = GeoMatchConstraintValue' "NZ"

pattern GMCVOM :: GeoMatchConstraintValue
pattern GMCVOM = GeoMatchConstraintValue' "OM"

pattern GMCVPA :: GeoMatchConstraintValue
pattern GMCVPA = GeoMatchConstraintValue' "PA"

pattern GMCVPE :: GeoMatchConstraintValue
pattern GMCVPE = GeoMatchConstraintValue' "PE"

pattern GMCVPF :: GeoMatchConstraintValue
pattern GMCVPF = GeoMatchConstraintValue' "PF"

pattern GMCVPG :: GeoMatchConstraintValue
pattern GMCVPG = GeoMatchConstraintValue' "PG"

pattern GMCVPH :: GeoMatchConstraintValue
pattern GMCVPH = GeoMatchConstraintValue' "PH"

pattern GMCVPK :: GeoMatchConstraintValue
pattern GMCVPK = GeoMatchConstraintValue' "PK"

pattern GMCVPL :: GeoMatchConstraintValue
pattern GMCVPL = GeoMatchConstraintValue' "PL"

pattern GMCVPM :: GeoMatchConstraintValue
pattern GMCVPM = GeoMatchConstraintValue' "PM"

pattern GMCVPN :: GeoMatchConstraintValue
pattern GMCVPN = GeoMatchConstraintValue' "PN"

pattern GMCVPR :: GeoMatchConstraintValue
pattern GMCVPR = GeoMatchConstraintValue' "PR"

pattern GMCVPS :: GeoMatchConstraintValue
pattern GMCVPS = GeoMatchConstraintValue' "PS"

pattern GMCVPT :: GeoMatchConstraintValue
pattern GMCVPT = GeoMatchConstraintValue' "PT"

pattern GMCVPW :: GeoMatchConstraintValue
pattern GMCVPW = GeoMatchConstraintValue' "PW"

pattern GMCVPY :: GeoMatchConstraintValue
pattern GMCVPY = GeoMatchConstraintValue' "PY"

pattern GMCVQA :: GeoMatchConstraintValue
pattern GMCVQA = GeoMatchConstraintValue' "QA"

pattern GMCVRE :: GeoMatchConstraintValue
pattern GMCVRE = GeoMatchConstraintValue' "RE"

pattern GMCVRO :: GeoMatchConstraintValue
pattern GMCVRO = GeoMatchConstraintValue' "RO"

pattern GMCVRS :: GeoMatchConstraintValue
pattern GMCVRS = GeoMatchConstraintValue' "RS"

pattern GMCVRU :: GeoMatchConstraintValue
pattern GMCVRU = GeoMatchConstraintValue' "RU"

pattern GMCVRW :: GeoMatchConstraintValue
pattern GMCVRW = GeoMatchConstraintValue' "RW"

pattern GMCVSA :: GeoMatchConstraintValue
pattern GMCVSA = GeoMatchConstraintValue' "SA"

pattern GMCVSB :: GeoMatchConstraintValue
pattern GMCVSB = GeoMatchConstraintValue' "SB"

pattern GMCVSC :: GeoMatchConstraintValue
pattern GMCVSC = GeoMatchConstraintValue' "SC"

pattern GMCVSD :: GeoMatchConstraintValue
pattern GMCVSD = GeoMatchConstraintValue' "SD"

pattern GMCVSE :: GeoMatchConstraintValue
pattern GMCVSE = GeoMatchConstraintValue' "SE"

pattern GMCVSG :: GeoMatchConstraintValue
pattern GMCVSG = GeoMatchConstraintValue' "SG"

pattern GMCVSH :: GeoMatchConstraintValue
pattern GMCVSH = GeoMatchConstraintValue' "SH"

pattern GMCVSI :: GeoMatchConstraintValue
pattern GMCVSI = GeoMatchConstraintValue' "SI"

pattern GMCVSJ :: GeoMatchConstraintValue
pattern GMCVSJ = GeoMatchConstraintValue' "SJ"

pattern GMCVSK :: GeoMatchConstraintValue
pattern GMCVSK = GeoMatchConstraintValue' "SK"

pattern GMCVSL :: GeoMatchConstraintValue
pattern GMCVSL = GeoMatchConstraintValue' "SL"

pattern GMCVSM :: GeoMatchConstraintValue
pattern GMCVSM = GeoMatchConstraintValue' "SM"

pattern GMCVSN :: GeoMatchConstraintValue
pattern GMCVSN = GeoMatchConstraintValue' "SN"

pattern GMCVSO :: GeoMatchConstraintValue
pattern GMCVSO = GeoMatchConstraintValue' "SO"

pattern GMCVSR :: GeoMatchConstraintValue
pattern GMCVSR = GeoMatchConstraintValue' "SR"

pattern GMCVSS :: GeoMatchConstraintValue
pattern GMCVSS = GeoMatchConstraintValue' "SS"

pattern GMCVST :: GeoMatchConstraintValue
pattern GMCVST = GeoMatchConstraintValue' "ST"

pattern GMCVSV :: GeoMatchConstraintValue
pattern GMCVSV = GeoMatchConstraintValue' "SV"

pattern GMCVSX :: GeoMatchConstraintValue
pattern GMCVSX = GeoMatchConstraintValue' "SX"

pattern GMCVSY :: GeoMatchConstraintValue
pattern GMCVSY = GeoMatchConstraintValue' "SY"

pattern GMCVSZ :: GeoMatchConstraintValue
pattern GMCVSZ = GeoMatchConstraintValue' "SZ"

pattern GMCVTC :: GeoMatchConstraintValue
pattern GMCVTC = GeoMatchConstraintValue' "TC"

pattern GMCVTD :: GeoMatchConstraintValue
pattern GMCVTD = GeoMatchConstraintValue' "TD"

pattern GMCVTF :: GeoMatchConstraintValue
pattern GMCVTF = GeoMatchConstraintValue' "TF"

pattern GMCVTG :: GeoMatchConstraintValue
pattern GMCVTG = GeoMatchConstraintValue' "TG"

pattern GMCVTH :: GeoMatchConstraintValue
pattern GMCVTH = GeoMatchConstraintValue' "TH"

pattern GMCVTJ :: GeoMatchConstraintValue
pattern GMCVTJ = GeoMatchConstraintValue' "TJ"

pattern GMCVTK :: GeoMatchConstraintValue
pattern GMCVTK = GeoMatchConstraintValue' "TK"

pattern GMCVTL :: GeoMatchConstraintValue
pattern GMCVTL = GeoMatchConstraintValue' "TL"

pattern GMCVTM :: GeoMatchConstraintValue
pattern GMCVTM = GeoMatchConstraintValue' "TM"

pattern GMCVTN :: GeoMatchConstraintValue
pattern GMCVTN = GeoMatchConstraintValue' "TN"

pattern GMCVTO :: GeoMatchConstraintValue
pattern GMCVTO = GeoMatchConstraintValue' "TO"

pattern GMCVTR :: GeoMatchConstraintValue
pattern GMCVTR = GeoMatchConstraintValue' "TR"

pattern GMCVTT :: GeoMatchConstraintValue
pattern GMCVTT = GeoMatchConstraintValue' "TT"

pattern GMCVTV :: GeoMatchConstraintValue
pattern GMCVTV = GeoMatchConstraintValue' "TV"

pattern GMCVTW :: GeoMatchConstraintValue
pattern GMCVTW = GeoMatchConstraintValue' "TW"

pattern GMCVTZ :: GeoMatchConstraintValue
pattern GMCVTZ = GeoMatchConstraintValue' "TZ"

pattern GMCVUA :: GeoMatchConstraintValue
pattern GMCVUA = GeoMatchConstraintValue' "UA"

pattern GMCVUG :: GeoMatchConstraintValue
pattern GMCVUG = GeoMatchConstraintValue' "UG"

pattern GMCVUM :: GeoMatchConstraintValue
pattern GMCVUM = GeoMatchConstraintValue' "UM"

pattern GMCVUS :: GeoMatchConstraintValue
pattern GMCVUS = GeoMatchConstraintValue' "US"

pattern GMCVUY :: GeoMatchConstraintValue
pattern GMCVUY = GeoMatchConstraintValue' "UY"

pattern GMCVUZ :: GeoMatchConstraintValue
pattern GMCVUZ = GeoMatchConstraintValue' "UZ"

pattern GMCVVA :: GeoMatchConstraintValue
pattern GMCVVA = GeoMatchConstraintValue' "VA"

pattern GMCVVC :: GeoMatchConstraintValue
pattern GMCVVC = GeoMatchConstraintValue' "VC"

pattern GMCVVE :: GeoMatchConstraintValue
pattern GMCVVE = GeoMatchConstraintValue' "VE"

pattern GMCVVG :: GeoMatchConstraintValue
pattern GMCVVG = GeoMatchConstraintValue' "VG"

pattern GMCVVI :: GeoMatchConstraintValue
pattern GMCVVI = GeoMatchConstraintValue' "VI"

pattern GMCVVN :: GeoMatchConstraintValue
pattern GMCVVN = GeoMatchConstraintValue' "VN"

pattern GMCVVU :: GeoMatchConstraintValue
pattern GMCVVU = GeoMatchConstraintValue' "VU"

pattern GMCVWF :: GeoMatchConstraintValue
pattern GMCVWF = GeoMatchConstraintValue' "WF"

pattern GMCVWS :: GeoMatchConstraintValue
pattern GMCVWS = GeoMatchConstraintValue' "WS"

pattern GMCVYE :: GeoMatchConstraintValue
pattern GMCVYE = GeoMatchConstraintValue' "YE"

pattern GMCVYT :: GeoMatchConstraintValue
pattern GMCVYT = GeoMatchConstraintValue' "YT"

pattern GMCVZA :: GeoMatchConstraintValue
pattern GMCVZA = GeoMatchConstraintValue' "ZA"

pattern GMCVZM :: GeoMatchConstraintValue
pattern GMCVZM = GeoMatchConstraintValue' "ZM"

pattern GMCVZW :: GeoMatchConstraintValue
pattern GMCVZW = GeoMatchConstraintValue' "ZW"

{-# COMPLETE
  GMCVAD,
  GMCVAE,
  GMCVAF,
  GMCVAG,
  GMCVAI,
  GMCVAL,
  GMCVAM,
  GMCVAO,
  GMCVAQ,
  GMCVAR,
  GMCVAS,
  GMCVAT,
  GMCVAU,
  GMCVAW,
  GMCVAX,
  GMCVAZ,
  GMCVBA,
  GMCVBB,
  GMCVBD,
  GMCVBE,
  GMCVBF,
  GMCVBG,
  GMCVBH,
  GMCVBI,
  GMCVBJ,
  GMCVBL,
  GMCVBM,
  GMCVBN,
  GMCVBO,
  GMCVBQ,
  GMCVBR,
  GMCVBS,
  GMCVBT,
  GMCVBV,
  GMCVBW,
  GMCVBY,
  GMCVBZ,
  GMCVCA,
  GMCVCC,
  GMCVCD,
  GMCVCF,
  GMCVCG,
  GMCVCH,
  GMCVCI,
  GMCVCK,
  GMCVCL,
  GMCVCM,
  GMCVCN,
  GMCVCO,
  GMCVCR,
  GMCVCU,
  GMCVCV,
  GMCVCW,
  GMCVCX,
  GMCVCY,
  GMCVCZ,
  GMCVDE,
  GMCVDJ,
  GMCVDK,
  GMCVDM,
  GMCVDO,
  GMCVDZ,
  GMCVEC,
  GMCVEE,
  GMCVEG,
  GMCVEH,
  GMCVER,
  GMCVES,
  GMCVET,
  GMCVFI,
  GMCVFJ,
  GMCVFK,
  GMCVFM,
  GMCVFO,
  GMCVFR,
  GMCVGA,
  GMCVGB,
  GMCVGD,
  GMCVGE,
  GMCVGF,
  GMCVGG,
  GMCVGH,
  GMCVGI,
  GMCVGL,
  GMCVGM,
  GMCVGN,
  GMCVGP,
  GMCVGQ,
  GMCVGR,
  GMCVGS,
  GMCVGT,
  GMCVGU,
  GMCVGW,
  GMCVGY,
  GMCVHK,
  GMCVHM,
  GMCVHN,
  GMCVHR,
  GMCVHT,
  GMCVHU,
  GMCVIE,
  GMCVIL,
  GMCVIM,
  GMCVIN,
  GMCVIO,
  GMCVIQ,
  GMCVIR,
  GMCVIS,
  GMCVIT,
  GMCVId,
  GMCVJE,
  GMCVJM,
  GMCVJO,
  GMCVJP,
  GMCVKE,
  GMCVKG,
  GMCVKH,
  GMCVKI,
  GMCVKM,
  GMCVKN,
  GMCVKP,
  GMCVKR,
  GMCVKW,
  GMCVKY,
  GMCVKZ,
  GMCVLA,
  GMCVLB,
  GMCVLC,
  GMCVLI,
  GMCVLK,
  GMCVLR,
  GMCVLS,
  GMCVLT,
  GMCVLU,
  GMCVLV,
  GMCVLY,
  GMCVMA,
  GMCVMC,
  GMCVMD,
  GMCVME,
  GMCVMF,
  GMCVMG,
  GMCVMH,
  GMCVMK,
  GMCVML,
  GMCVMM,
  GMCVMN,
  GMCVMO,
  GMCVMP,
  GMCVMQ,
  GMCVMR,
  GMCVMS,
  GMCVMT,
  GMCVMU,
  GMCVMV,
  GMCVMW,
  GMCVMX,
  GMCVMY,
  GMCVMZ,
  GMCVNA,
  GMCVNC,
  GMCVNE,
  GMCVNF,
  GMCVNG,
  GMCVNI,
  GMCVNL,
  GMCVNO,
  GMCVNP,
  GMCVNR,
  GMCVNU,
  GMCVNZ,
  GMCVOM,
  GMCVPA,
  GMCVPE,
  GMCVPF,
  GMCVPG,
  GMCVPH,
  GMCVPK,
  GMCVPL,
  GMCVPM,
  GMCVPN,
  GMCVPR,
  GMCVPS,
  GMCVPT,
  GMCVPW,
  GMCVPY,
  GMCVQA,
  GMCVRE,
  GMCVRO,
  GMCVRS,
  GMCVRU,
  GMCVRW,
  GMCVSA,
  GMCVSB,
  GMCVSC,
  GMCVSD,
  GMCVSE,
  GMCVSG,
  GMCVSH,
  GMCVSI,
  GMCVSJ,
  GMCVSK,
  GMCVSL,
  GMCVSM,
  GMCVSN,
  GMCVSO,
  GMCVSR,
  GMCVSS,
  GMCVST,
  GMCVSV,
  GMCVSX,
  GMCVSY,
  GMCVSZ,
  GMCVTC,
  GMCVTD,
  GMCVTF,
  GMCVTG,
  GMCVTH,
  GMCVTJ,
  GMCVTK,
  GMCVTL,
  GMCVTM,
  GMCVTN,
  GMCVTO,
  GMCVTR,
  GMCVTT,
  GMCVTV,
  GMCVTW,
  GMCVTZ,
  GMCVUA,
  GMCVUG,
  GMCVUM,
  GMCVUS,
  GMCVUY,
  GMCVUZ,
  GMCVVA,
  GMCVVC,
  GMCVVE,
  GMCVVG,
  GMCVVI,
  GMCVVN,
  GMCVVU,
  GMCVWF,
  GMCVWS,
  GMCVYE,
  GMCVYT,
  GMCVZA,
  GMCVZM,
  GMCVZW,
  GeoMatchConstraintValue'
  #-}
